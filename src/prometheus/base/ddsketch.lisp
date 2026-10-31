(in-package #:prometheus)

;;; DDSketch (Masson et al., VLDB 2019): relative-error quantiles, merged by
;;; adding counters. Fixed memory, no allocation on observe or read.
;;; Design notes: docs/quantile-summary.md.

(deftype counter-word () #+sbcl 'sb-ext:word #-sbcl '(unsigned-byte 64))
(deftype sketch-counts () '(simple-array counter-word (*)))
(deftype quantile-buffer () '(simple-array double-float (*)))

#+sbcl
(progn
  (declaim (inline %d->w %w->d))
  (defun %d->w (d)
    (declare (double-float d))
    (ldb (byte 64 0) (sb-kernel:double-float-bits d)))
  (defun %w->d (w)
    (declare (type sb-ext:word w))
    (sb-kernel:make-double-float (- (ldb (byte 32 32) w) (if (logbitp 63 w) (ash 1 32) 0))
                                 (ldb (byte 32 0) w))))

(defstruct (ddsketch (:constructor %make-ddsketch)
                     (:copier nil))
  (relative-error 0d0 :type double-float :read-only t)
  (gamma 0d0 :type double-float :read-only t)
  (inv-log-gamma 0d0 :type double-float :read-only t)
  (min-value 0d0 :type double-float :read-only t)
  (max-value 0d0 :type double-float :read-only t)
  ;; key of COUNTS[1]; COUNTS[0] is underflow, COUNTS[last] is overflow
  (min-key 0 :type fixnum :read-only t)
  (counts (make-array 0 :element-type 'counter-word) :type sketch-counts :read-only t)
  #+sbcl (sum-bits 0 :type sb-ext:word)
  #+sbcl (min-bits 0 :type sb-ext:word)
  #+sbcl (max-bits 0 :type sb-ext:word)
  #-sbcl (%sum 0d0 :type double-float)
  #-sbcl (%min 0d0 :type double-float)
  #-sbcl (%max 0d0 :type double-float)
  #-sbcl (lock (bt:make-lock "prometheus ddsketch lock") :read-only t))

(declaim (inline %sketch-sum %sketch-min %sketch-max))
(defun %sketch-sum (s) #+sbcl (%w->d (ddsketch-sum-bits s)) #-sbcl (ddsketch-%sum s))
(defun %sketch-min (s) #+sbcl (%w->d (ddsketch-min-bits s)) #-sbcl (ddsketch-%min s))
(defun %sketch-max (s) #+sbcl (%w->d (ddsketch-max-bits s)) #-sbcl (ddsketch-%max s))

(defmacro %with-sketch-locked ((sketch) &body body)
  #+sbcl (declare (ignore sketch))
  #+sbcl `(progn ,@body)
  #-sbcl `(bt:with-lock-held ((ddsketch-lock ,sketch)) ,@body))

(defun ddsketch-reset (sketch)
  "Empty SKETCH in place, keeping its buffers.  Meant for reusing a merge
target; not atomic with respect to concurrent observations."
  (declare (type ddsketch sketch))
  (%with-sketch-locked (sketch)
    (fill (ddsketch-counts sketch) 0)
    #+sbcl
    (setf (ddsketch-sum-bits sketch) (%d->w 0d0)
          (ddsketch-min-bits sketch) (%d->w most-positive-double-float)
          (ddsketch-max-bits sketch) (%d->w 0d0))
    #-sbcl
    (setf (ddsketch-%sum sketch) 0d0
          (ddsketch-%min sketch) most-positive-double-float
          (ddsketch-%max sketch) 0d0))
  sketch)

(defun %sketch-key (inv-log-gamma value)
  (ceiling (* (log (coerce value 'double-float)) inv-log-gamma)))

(defun check-ddsketch-parameters (relative-error min-value max-value &optional (shards 1))
  "Signal INVALID-VALUE-ERROR unless these would make a valid sketch."
  (unless (and (realp relative-error) (< 0 relative-error 1))
    (error 'invalid-value-error :value relative-error
                                :reason "relative error should be in (0, 1)"))
  (unless (and (realp min-value) (realp max-value) (< 1d-300 min-value max-value 1d300))
    (error 'invalid-value-error :value (cons min-value max-value)
                                :reason "expected 1d-300 < min-value < max-value < 1d300"))
  (unless (and (typep shards '(integer 1 1024)) (= 1 (logcount shards)))
    (error 'invalid-value-error :value shards :reason "shards should be a power of two in [1, 1024]")))

(defun make-ddsketch (&key (relative-error 0.01d0) (min-value 1d-9) (max-value 1d9))
  "Create an empty sketch.  Quantile estimates for values within
[MIN-VALUE, MAX-VALUE] are within RELATIVE-ERROR of the true value.  Memory is
fixed at creation: about log(MAX-VALUE/MIN-VALUE) / (2 RELATIVE-ERROR) words,
~2100 words (17KB) for the defaults."
  (check-ddsketch-parameters relative-error min-value max-value)
  (let* ((alpha (coerce relative-error 'double-float))
         (gamma (/ (+ 1 alpha) (- 1 alpha)))
         (inv-log-gamma (/ 1 (log gamma)))
         (min-key (%sketch-key inv-log-gamma min-value))
         (max-key (%sketch-key inv-log-gamma max-value))
         (size (+ 2 (- max-key min-key) 1)))
    (ddsketch-reset
     (%make-ddsketch :relative-error alpha
                     :gamma gamma
                     :inv-log-gamma inv-log-gamma
                     :min-value (coerce min-value 'double-float)
                     :max-value (coerce max-value 'double-float)
                     :min-key min-key
                     :counts (make-array size :element-type 'counter-word :initial-element 0)))))

(defun ddsketch-compatible-p (a b)
  (and (= (ddsketch-gamma a) (ddsketch-gamma b))
       (= (ddsketch-min-key a) (ddsketch-min-key b))
       (= (length (ddsketch-counts a)) (length (ddsketch-counts b)))))

(declaim (inline %sketch-index))
(defun %sketch-index (sketch d)
  "Counter index for D, a finite non-negative double."
  (declare (type ddsketch sketch) (type (double-float 0d0) d))
  (let ((last (1- (length (ddsketch-counts sketch)))))
    (cond ((< d (ddsketch-min-value sketch)) 0)
          ((> d (ddsketch-max-value sketch)) last)
          (t
           ;; D is in [min-value, max-value] here, both in (1d-300, 1d300)
           (let ((x (* (log (the (double-float 1d-300 1d300) d))
                       (ddsketch-inv-log-gamma sketch))))
             (declare (type (double-float -1d8 1d8) x))
             (min (1- last)
                  (max 1 (1+ (- (ceiling x) (ddsketch-min-key sketch))))))))))

#+sbcl
(defmacro %cas-double-bits ((old accessor sketch) new-form &key (when t))
  "Replace the double stored as bits in (ACCESSOR SKETCH) with NEW-FORM,
evaluated with OLD bound to the current double, as long as WHEN holds."
  (let ((bits (gensym "BITS")))
    `(loop
       (let* ((,bits (,accessor ,sketch))
              (,old (%w->d ,bits)))
         (declare (ignorable ,old))
         (unless ,when (return))
         (when (= ,bits (sb-ext:compare-and-swap (,accessor ,sketch) ,bits (%d->w ,new-form)))
           (return))))))

(declaim (inline %sketch-add))
(defun %sketch-add (sketch index count sum min max)
  "Add COUNT observations at counter INDEX, and fold SUM, MIN, MAX in."
  (declare (type ddsketch sketch) (type fixnum index) (type counter-word count)
           (type double-float sum min max))
  #+sbcl
  (progn
    (unless (zerop count)
      (sb-ext:atomic-incf (aref (ddsketch-counts sketch) index) count))
    (%cas-double-bits (old ddsketch-sum-bits sketch) (+ old sum))
    (%cas-double-bits (old ddsketch-min-bits sketch) min :when (< min old))
    (%cas-double-bits (old ddsketch-max-bits sketch) max :when (> max old)))
  #-sbcl
  (%with-sketch-locked (sketch)
    (incf (aref (ddsketch-counts sketch) index) count)
    (incf (ddsketch-%sum sketch) sum)
    (when (< min (ddsketch-%min sketch)) (setf (ddsketch-%min sketch) min))
    (when (> max (ddsketch-%max sketch)) (setf (ddsketch-%max sketch) max)))
  nil)

(declaim (inline %ddsketch-observe-double))
(defun %ddsketch-observe-double (sketch d)
  (declare (type ddsketch sketch) (type (double-float 0d0) d) (optimize speed))
  (%sketch-add sketch (%sketch-index sketch d) 1 d d d))

(defun ddsketch-observe (sketch value)
  "Record VALUE, a finite non-negative real number.  Allocates nothing."
  (declare (type ddsketch sketch) (optimize speed))
  ;; also rejects NaN, which compares false to everything
  (unless (and (realp value) (<= 0 value most-positive-double-float))
    (error 'invalid-value-error :value value
                                :reason "value should be a finite non-negative real number"))
  ;; generic COERCE returns a boxed double
  (typecase value
    (double-float (%ddsketch-observe-double sketch value))
    (fixnum (%ddsketch-observe-double sketch (coerce value 'double-float)))
    (single-float (%ddsketch-observe-double sketch (coerce value 'double-float)))
    (t (%ddsketch-observe-double sketch (coerce value 'double-float))))
  sketch)

(defun ddsketch-count (sketch)
  (declare (type ddsketch sketch) (optimize speed))
  (let ((total 0))
    (declare (type counter-word total))
    (loop for c of-type counter-word across (ddsketch-counts sketch)
          do (setf total (logand (+ total c) #xFFFFFFFFFFFFFFFF)))
    total))

(defun ddsketch-sum (sketch)
  "Sum of observations, a double-float."
  (%sketch-sum sketch))

(defun ddsketch-min (sketch)
  "Smallest observation, or NIL when empty."
  (unless (zerop (ddsketch-count sketch)) (%sketch-min sketch)))

(defun ddsketch-max (sketch)
  "Largest observation, or NIL when empty."
  (unless (zerop (ddsketch-count sketch)) (%sketch-max sketch)))

(declaim (inline %sketch-bucket-value))
(defun %sketch-bucket-value (sketch index)
  (declare (type ddsketch sketch) (type fixnum index))
  (let ((last (1- (length (ddsketch-counts sketch)))))
    (cond ((= index 0) (ddsketch-min-value sketch))
          ((= index last) (ddsketch-max-value sketch))
          (t (let ((gamma (ddsketch-gamma sketch))
                   (key (+ (ddsketch-min-key sketch) (1- index))))
               (/ (* 2 (exp (the (double-float -1d3 1d3)
                                 (/ (coerce key 'double-float) (ddsketch-inv-log-gamma sketch)))))
                  (+ gamma 1)))))))

(defun check-quantile-buffers (qs out)
  (unless (and (typep qs 'quantile-buffer) (typep out 'quantile-buffer)
               (= (length qs) (length out)))
    (error 'invalid-value-error :value qs
                                :reason "quantiles and output should be double-float vectors of the same length"))
  (let ((qs qs))
    (declare (type quantile-buffer qs) (optimize speed))
    (loop with previous of-type double-float = 0d0
          for q of-type double-float across qs
          unless (and (<= 0d0 q 1d0) (<= previous q))
            do (error 'invalid-value-error :value qs
                                           :reason "quantiles should be ascending, in [0, 1]")
          do (setf previous q))))

(defun %sketches-quantiles-into (sketches qs out)
  "Write into OUT the quantiles QS (ascending) of the union of SKETCHES, a
simple-vector of compatible sketches, in one pass over the counters and
without allocating.  Returns NIL, leaving OUT alone, when there are no
observations; T otherwise."
  (declare (type simple-vector sketches) (type quantile-buffer qs out)
           (optimize speed))
  (let ((size (length (ddsketch-counts (the ddsketch (svref sketches 0)))))
        (total 0)
        (min most-positive-double-float)
        (max 0d0))
    (declare (type (unsigned-byte 62) total) (type double-float min max))
    (loop for sketch of-type ddsketch across sketches
          do (setf total (+ total (the (unsigned-byte 58) (ddsketch-count sketch))))
             (setf min (min min (%sketch-min sketch))
                   max (max max (%sketch-max sketch))))
    (when (zerop total)
      (return-from %sketches-quantiles-into nil))
    (let ((n (length qs))
          (qi 0)
          (seen 0)
          (last-rank (coerce (1- total) 'double-float)))
      (declare (type fixnum qi) (type (unsigned-byte 62) seen))
      (loop while (and (< qi n) (<= (aref qs qi) 0d0))
            do (setf (aref out qi) min) (incf qi))
      (dotimes (index size)
        (when (or (= qi n) (>= (aref qs qi) 1d0))
          (return))
        (loop for sketch of-type ddsketch across sketches
              do (setf seen (+ seen (the (unsigned-byte 58) (aref (ddsketch-counts sketch) index)))))
        (loop while (and (< qi n)
                         (< (aref qs qi) 1d0)
                         (> (coerce seen 'double-float) (* (aref qs qi) last-rank)))
              do (let ((estimate (%sketch-bucket-value (svref sketches 0) index)))
                   (setf (aref out qi) (cond ((< estimate min) min)
                                             ((> estimate max) max)
                                             (t estimate))))
                 (incf qi)))
      ;; q = 1, or counters grew during the walk
      (loop while (< qi n)
            do (setf (aref out qi) max) (incf qi))
      t)))

(defun ddsketch-quantiles-into (sketch qs out)
  "Write the quantiles QS (an ascending (SIMPLE-ARRAY DOUBLE-FLOAT (*)) in
[0, 1]) of SKETCH into OUT, a double-float array of the same length.  Returns
NIL and leaves OUT alone when SKETCH is empty.  Allocates nothing."
  (check-quantile-buffers qs out)
  (let ((sketches (vector sketch)))
    (declare (dynamic-extent sketches))
    (%sketches-quantiles-into sketches qs out)))

(defun ddsketch-quantile (sketch q)
  "Estimate the Q-quantile, 0 <= Q <= 1.  Returns NIL for an empty sketch.
The estimate is clamped to the observed [min, max]."
  (unless (and (realp q) (<= 0 q 1))
    (error 'invalid-value-error :value q :reason "quantile should be in [0, 1]"))
  (let ((sketches (vector sketch))
        (qs (make-array 1 :element-type 'double-float :initial-element (coerce q 'double-float)))
        (out (make-array 1 :element-type 'double-float :initial-element 0d0)))
    (declare (dynamic-extent sketches qs out))
    (when (%sketches-quantiles-into sketches qs out)
      (aref out 0))))

(defun ddsketch-merge (into from)
  "Add every observation of FROM to INTO.  Both must have been made with the
same parameters.  Safe to call while INTO and FROM are being observed into,
though a concurrent observation into FROM may or may not be included.
Allocates nothing."
  (unless (ddsketch-compatible-p into from)
    (error 'invalid-value-error :value from
                                :reason "sketches were created with different parameters"))
  (let ((src (ddsketch-counts from)))
    (dotimes (index (length src))
      (let ((c (aref src index)))
        (unless (zerop c)
          #+sbcl (sb-ext:atomic-incf (aref (ddsketch-counts into) index) c)
          #-sbcl (%with-sketch-locked (into) (incf (aref (ddsketch-counts into) index) c)))))
    (%sketch-add into 0 0 (%sketch-sum from) (%sketch-min from) (%sketch-max from)))
  into)

(defun ddsketch-empty-copy (sketch)
  "A new empty sketch with SKETCH's parameters."
  (make-ddsketch :relative-error (ddsketch-relative-error sketch)
                 :min-value (ddsketch-min-value sketch)
                 :max-value (ddsketch-max-value sketch)))

(defun ddsketch-merged (sketch &rest more)
  "A new sketch holding the observations of SKETCH and every one of MORE.
To reuse a buffer instead, DDSKETCH-RESET a sketch and DDSKETCH-MERGE into it."
  (let ((result (ddsketch-empty-copy sketch)))
    (ddsketch-merge result sketch)
    (dolist (s more result)
      (ddsketch-merge result s))))

;;; One sketch per thread hash, combined on read. Avoids cache-line contention
;;; on counters, sum and extremes.

(defstruct (sharded-ddsketch (:constructor %make-sharded-ddsketch)
                             (:copier nil))
  (shards #() :type simple-vector :read-only t))

(defun make-sharded-ddsketch (&key (shards 16) (relative-error 0.01d0) (min-value 1d-9) (max-value 1d9))
  "Like MAKE-DDSKETCH, spread over SHARDS (a power of two) sub-sketches, all
allocated now.  Only SBCL shards; elsewhere one locked sketch is used."
  (check-ddsketch-parameters relative-error min-value max-value shards)
  (let ((vector (make-array #+sbcl shards #-sbcl 1)))
    (dotimes (i (length vector))
      (setf (svref vector i) (make-ddsketch :relative-error relative-error
                                            :min-value min-value :max-value max-value)))
    (%make-sharded-ddsketch :shards vector)))

(declaim (inline %current-shard-index))
(defun %current-shard-index (mask)
  ;; OS thread ids are sequential: a pool of up to SHARDS threads gets one
  ;; shard each, where an address hash would collide.
  (declare (type (unsigned-byte 10) mask))
  #+sbcl (logand (sb-thread:thread-os-tid sb-thread:*current-thread*) mask)
  #-sbcl (progn mask 0))

(defun sharded-ddsketch-observe (sketch value)
  "Record VALUE in the calling thread's shard.  Allocates nothing."
  (let ((shards (sharded-ddsketch-shards sketch)))
    (ddsketch-observe (svref shards (%current-shard-index (1- (length shards)))) value))
  sketch)

(defun sharded-ddsketch-quantiles-into (sketch qs out)
  "DDSKETCH-QUANTILES-INTO over all shards, in place."
  (check-quantile-buffers qs out)
  (%sketches-quantiles-into (sharded-ddsketch-shards sketch) qs out))

(defun sharded-ddsketch-quantile (sketch q)
  (unless (and (realp q) (<= 0 q 1))
    (error 'invalid-value-error :value q :reason "quantile should be in [0, 1]"))
  (let ((qs (make-array 1 :element-type 'double-float :initial-element (coerce q 'double-float)))
        (out (make-array 1 :element-type 'double-float :initial-element 0d0)))
    (declare (dynamic-extent qs out))
    (when (%sketches-quantiles-into (sharded-ddsketch-shards sketch) qs out)
      (aref out 0))))

(defun sharded-ddsketch-count (sketch)
  (loop for shard across (sharded-ddsketch-shards sketch)
        sum (ddsketch-count shard)))

(defun sharded-ddsketch-sum (sketch)
  (loop for shard across (sharded-ddsketch-shards sketch)
        sum (ddsketch-sum shard)))

(defun sharded-ddsketch-snapshot (sketch &optional into)
  "A DDSKETCH holding every observation made so far, all shards merged.  With
INTO (a compatible sketch) it is reset and reused instead of allocating."
  (let* ((shards (sharded-ddsketch-shards sketch))
         (result (if into
                     (ddsketch-reset into)
                     (ddsketch-empty-copy (svref shards 0)))))
    (loop for shard across shards
          do (ddsketch-merge result shard))
    result))
