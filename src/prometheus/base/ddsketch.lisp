(in-package #:prometheus)

;;; DDSketch (Masson et al., VLDB 2019): relative-error quantiles, merged by
;;; adding counters. Fixed memory, no allocation on observe or read.
;;; Design notes: docs/quantile-summary.md.

(deftype quantile-buffer () '(simple-array double-float (*)))
(deftype sketch-data () '(simple-array double-float (*)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +sketch-max-buckets+ 65536
    "Upper bound on buckets per sketch, about 1MB per shard.")
  (defconstant +sketch-max-key+ (expt 2 50)))

;; small index types keep all index arithmetic in fixnums
(deftype sketch-bucket () `(integer 0 (,+sketch-max-buckets+)))
(deftype sketch-pair () `(integer 0 (,(+ 512 1 +sketch-max-buckets+))))
(deftype sketch-word () `(integer 0 (,(* 2 (+ 512 1 +sketch-max-buckets+)))))

;;; DATA layout, in doubles:
;;;   [guard | min max | count0 sum0 | count1 sum1 | ... | guard]
;;; Pairs are 16-byte aligned and updated with one 16-byte CAS, so a bucket's
;;; count and sum change together. Bucket 0 is underflow, the last is
;;; overflow. The guards keep other objects off the lines we write.
(defstruct (ddsketch (:constructor %make-ddsketch)
                     (:copier nil))
  "Single writer, or a value: a merge target or snapshot no longer written.
Concurrent observes and reads go through SHARDED-DDSKETCH."
  (relative-error 0d0 :type double-float :read-only t)
  (gamma 0d0 :type double-float :read-only t)
  (log-gamma 0d0 :type double-float :read-only t)
  (inv-log-gamma 0d0 :type double-float :read-only t)
  (min-value 0d0 :type double-float :read-only t)
  (max-value 0d0 :type double-float :read-only t)
  ;; key of bucket 1
  (min-key 0 :type (integer (#.(- +sketch-max-key+)) (#.+sketch-max-key+)) :read-only t)
  (buckets 0 :type (integer 0 #.+sketch-max-buckets+) :read-only t)
  ;; key range of in-range values, to bound the key computation
  (key-lo 0d0 :type double-float :read-only t)
  (key-hi 0d0 :type double-float :read-only t)
  ;; pair index of (min max); bucket B is pair BASE + 1 + B
  (base 0 :type (integer 0 (512)) :read-only t)
  ;; value reported for each bucket, shared by sketches with the same parameters
  (reps (make-array 0 :element-type 'double-float) :type sketch-data :read-only t)
  (data (make-array 0 :element-type 'double-float) :type sketch-data :read-only t)
  (lock (bt:make-lock "prometheus ddsketch lock") :read-only t))

(defmacro %with-sketch-locked ((sketch) &body body)
  #+prometheus-cas-pair (declare (ignore sketch))
  #+prometheus-cas-pair `(progn ,@body)
  #-prometheus-cas-pair `(bt:with-lock-held ((ddsketch-lock ,sketch)) ,@body))

(declaim (inline %pair-word)
         (ftype (function (sketch-pair) sketch-word) %pair-word))
(defun %pair-word (pair) (* 2 pair))

(declaim (inline %sketch-min %sketch-max))
(defun %sketch-min (s) (aref (ddsketch-data s) (%pair-word (ddsketch-base s))))
(defun %sketch-max (s) (aref (ddsketch-data s) (1+ (%pair-word (ddsketch-base s)))))

(declaim (inline %bucket-count %bucket-sum))
(defun %bucket-count (s b) (aref (ddsketch-data s) (%pair-word (+ (ddsketch-base s) 1 b))))
(defun %bucket-sum (s b) (aref (ddsketch-data s) (1+ (%pair-word (+ (ddsketch-base s) 1 b)))))

(defun ddsketch-reset (sketch)
  "Empty SKETCH in place, keeping its buffers. Meant for reusing a merge
target; not atomic with respect to concurrent observations."
  (declare (type ddsketch sketch))
  (let ((data (ddsketch-data sketch))
        (m (%pair-word (ddsketch-base sketch))))
    (bt:with-lock-held ((ddsketch-lock sketch))
      (fill data 0d0)
      (setf (aref data m) most-positive-double-float
            (aref data (1+ m)) 0d0)))
  sketch)

(defun %no-doubles () (make-array 0 :element-type 'double-float))

(defun %sketch-key (inv-log-gamma value)
  (ceiling (* (log (coerce value 'double-float)) inv-log-gamma)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; key = ceil(log(v) / log(gamma)) is off by up to ~1.5d-13 / alpha of a
  ;; bucket from rounding in LOG; above this that stays negligible
  (defconstant +sketch-min-relative-error+ 1d-6))

(defun %sketch-geometry (relative-error min-value max-value)
  "Validate the parameters and return gamma, 1/log(gamma), and the keys of
MIN-VALUE and MAX-VALUE."
  (unless (and (realp relative-error) (< 0 relative-error 1))
    (error 'invalid-value-error :value relative-error
                                :reason "relative error should be in (0, 1)"))
  (unless (and (realp min-value) (realp max-value) (< 1d-300 min-value max-value 1d300))
    (error 'invalid-value-error :value (cons min-value max-value)
                                :reason "expected 1d-300 < min-value < max-value < 1d300"))
  (flet ((fail (value reason)
           (error 'invalid-value-error :value value :reason reason)))
    (let ((alpha (coerce relative-error 'double-float)))
      ;; a rational just below 1 rounds to 1d0
      (unless (< alpha 1d0)
        (fail relative-error "relative error too close to 1 for double precision"))
      (unless (>= alpha +sketch-min-relative-error+)
        (fail relative-error (format nil "relative error below ~a, rounding would exceed it"
                                     +sketch-min-relative-error+)))
      (let ((gamma (/ (+ 1 alpha) (- 1 alpha))))
        (let* ((inv-log-gamma (/ 1 (log gamma)))
               (min-key (%sketch-key inv-log-gamma min-value))
               (max-key (%sketch-key inv-log-gamma max-value)))
          (unless (and (< (abs min-key) +sketch-max-key+) (< (abs max-key) +sketch-max-key+))
            (fail relative-error "relative error too small for this value range"))
          (unless (<= (+ 3 (- max-key min-key)) +sketch-max-buckets+)
            (fail (list relative-error min-value max-value)
                  (format nil "more than ~a buckets, raise relative error or narrow the range"
                          +sketch-max-buckets+)))
          (values gamma inv-log-gamma min-key max-key))))))

(defun check-ddsketch-parameters (relative-error min-value max-value &optional (shards 1))
  "Signal INVALID-VALUE-ERROR unless these would make a valid sketch."
  (%sketch-geometry relative-error min-value max-value)
  (check-shards shards))

(defun make-ddsketch (&key (relative-error 0.01d0) (min-value 1d-9) (max-value 1d9))
  "Create an empty sketch. Quantile estimates for values within
[MIN-VALUE, MAX-VALUE] are within RELATIVE-ERROR of the true value. Memory is
fixed at creation: two doubles per bucket, about log(MAX-VALUE/MIN-VALUE) /
(2 RELATIVE-ERROR) buckets, ~34KB for the defaults."
  (multiple-value-bind (gamma inv-log-gamma min-key max-key)
      (%sketch-geometry relative-error min-value max-value)
    (let* ((buckets (+ 2 (- max-key min-key) 1))
           ;; guard of whole lines, an even count of doubles keeps pairs aligned
           (guard (max 2 (* 2 (ceiling *cache-line-bytes* 16))))
           (data (make-array (+ guard 2 (* 2 buckets) guard)
                             :element-type 'double-float :initial-element 0d0)))
      (flet ((make (reps data)
               (%make-ddsketch :relative-error (coerce relative-error 'double-float)
                               :gamma gamma
                               :log-gamma (log gamma)
                               :inv-log-gamma inv-log-gamma
                               :min-value (coerce min-value 'double-float)
                               :max-value (coerce max-value 'double-float)
                               :min-key min-key
                               :buckets buckets
                               :key-lo (coerce (1- min-key) 'double-float)
                               :key-hi (coerce (1+ max-key) 'double-float)
                               :base (floor guard 2)
                               :reps reps
                               :data data)))
        ;; the table is built from a sketch without one
        (ddsketch-reset (make (%sketch-reps (make (%no-doubles) (%no-doubles))) data))))))

(defun %same-layout-p (a b)
  "Pairs sit at the same indices: the guard follows the cache line size at
creation, which may differ between images."
  (and (= (ddsketch-base a) (ddsketch-base b))
       (= (length (ddsketch-data a)) (length (ddsketch-data b)))))

(defun ddsketch-compatible-p (a b)
  "True when A and B bucket values the same way, so they can be merged. The
range matters too: underflow and overflow buckets report MIN-VALUE and
MAX-VALUE."
  (and (= (ddsketch-gamma a) (ddsketch-gamma b))
       (= (ddsketch-min-key a) (ddsketch-min-key b))
       (= (ddsketch-buckets a) (ddsketch-buckets b))
       (= (ddsketch-min-value a) (ddsketch-min-value b))
       (= (ddsketch-max-value a) (ddsketch-max-value b))))

(declaim (inline %sketch-index))
(defun %sketch-index (sketch d)
  "Bucket index for D, a finite non-negative double."
  (declare (type ddsketch sketch) (type (double-float 0d0) d))
  (let ((last (1- (ddsketch-buckets sketch))))
    (cond ((< d (ddsketch-min-value sketch)) 0)
          ((> d (ddsketch-max-value sketch)) last)
          (t
           ;; D is in [min-value, max-value] here, both in (1d-300, 1d300).
           ;; Clamping to the validated key range bounds CEILING.
           (let ((x (max (ddsketch-key-lo sketch)
                         (min (ddsketch-key-hi sketch)
                              (* (log (the (double-float 1d-300 1d300) d))
                                 (ddsketch-inv-log-gamma sketch))))))
             (declare (type (double-float -1.2d15 1.2d15) x))
             (the sketch-bucket
                  (min (1- last)
                       (max 1 (1+ (- (ceiling x) (ddsketch-min-key sketch)))))))))))

#+prometheus-cas-pair
(defmacro %cas-pair-loop ((a b data pair) (new-a new-b) &key (when t))
  "Replace pair PAIR of DATA with NEW-A, NEW-B, evaluated with A, B bound to
the current doubles, as long as WHEN holds. Retries until the swap lands."
  (let ((wa (gensym "WA")) (wb (gensym "WB")) (ra (gensym "RA")) (rb (gensym "RB")) (w (gensym "W")))
    `(let ((,w (%pair-word ,pair)))
       (loop
         (let* ((,a (aref ,data ,w))
                (,b (aref ,data (1+ ,w)))
                (,wa (%d->w ,a))
                (,wb (%d->w ,b)))
           (unless ,when (return))
           (multiple-value-bind (,ra ,rb) (%cas-pair ,data ,pair ,wa ,wb (%d->w ,new-a) (%d->w ,new-b))
             (when (and (= ,ra ,wa) (= ,rb ,wb))
               (return))))))))

(declaim (inline %sketch-add))
(defun %sketch-add (sketch bucket count sum min max)
  "Fold MIN, MAX in, then add COUNT and SUM to BUCKET. Extrema go first so a
reader that sees the count also sees real extrema."
  (declare (type ddsketch sketch) (type sketch-bucket bucket) (type double-float count sum min max))
  (let ((data (ddsketch-data sketch))
        (base (ddsketch-base sketch)))
    #+prometheus-cas-pair
    (progn
      (%cas-pair-loop (lo hi data base) ((min lo min) (max hi max))
                      :when (or (< min lo) (> max hi)))
      (%cas-pair-loop (c s data (+ base 1 bucket)) ((+ c count) (+ s sum))))
    #-prometheus-cas-pair
    (%with-sketch-locked (sketch)
      (let ((m (%pair-word base))
            (w (%pair-word (+ base 1 bucket))))
        (when (< min (aref data m)) (setf (aref data m) min))
        (when (> max (aref data (1+ m))) (setf (aref data (1+ m)) max))
        (incf (aref data w) count)
        (incf (aref data (1+ w)) sum))))
  nil)

(declaim (inline %ddsketch-observe-double))
(defun %ddsketch-observe-double (sketch d)
  (declare (type ddsketch sketch) (type (double-float 0d0) d) (optimize speed))
  (%sketch-add sketch (%sketch-index sketch d) 1d0 d d d))

(defun %bad-sketch-value (value)
  (error 'invalid-value-error :value value
                              :reason "value should be a finite non-negative real number"))

(defun ddsketch-observe (sketch value)
  "Record VALUE, a finite non-negative real number. Allocates nothing."
  (declare (type ddsketch sketch) (optimize speed))
  ;; check per type: a generic comparison would run before the TYPECASE.
  ;; NaN fails every comparison, so it is rejected too.
  (typecase value
    (double-float
     (if (<= 0d0 value most-positive-double-float)
         (%ddsketch-observe-double sketch value)
         (%bad-sketch-value value)))
    (fixnum
     (if (>= value 0)
         (%ddsketch-observe-double sketch (coerce value 'double-float))
         (%bad-sketch-value value)))
    (t
     (if (and (realp value) (<= 0 value most-positive-double-float))
         (%ddsketch-observe-double sketch (coerce value 'double-float))
         (%bad-sketch-value value))))
  sketch)

(declaim (inline %sketch-total))
(defun %sketch-total (sketch)
  (declare (type ddsketch sketch) (optimize speed))
  (let ((total 0d0))
    (declare (type double-float total))
    (dotimes (b (ddsketch-buckets sketch) total)
      (incf total (%bucket-count sketch b)))))

(declaim (inline %count->integer))
(defun %count->integer (count)
  ;; counts stay exact integers in a double up to 2^53
  (values (truncate (the (double-float 0d0 9.007199254740992d15) count))))

(defun ddsketch-count (sketch)
  (%count->integer (%sketch-total sketch)))

(defun ddsketch-sum (sketch)
  "Sum of observations, a double-float."
  (declare (type ddsketch sketch) (optimize speed))
  (let ((sum 0d0))
    (declare (type double-float sum))
    (dotimes (b (ddsketch-buckets sketch) sum)
      (incf sum (%bucket-sum sketch b)))))

(defun ddsketch-min (sketch)
  "Smallest observation, or NIL when empty. On a sketch still observed into
it may include an observation not counted yet."
  (unless (zerop (%sketch-total sketch)) (%sketch-min sketch)))

(defun ddsketch-max (sketch)
  "Largest observation, or NIL when empty."
  (unless (zerop (%sketch-total sketch)) (%sketch-max sketch)))

(defun %build-sketch-reps (proto)
  (let* ((buckets (ddsketch-buckets proto))
         (last (1- buckets))
         (gamma (ddsketch-gamma proto))
         ;; 2 gamma^key / (gamma + 1), in log space so gamma^key cannot overflow
         (log-mid (log (/ 2 (+ gamma 1))))
         (reps (make-array buckets :element-type 'double-float :initial-element 0d0)))
    (dotimes (b buckets reps)
      (setf (aref reps b)
            (cond ((= b 0) (ddsketch-min-value proto))
                  ((= b last) (ddsketch-max-value proto))
                  (t (exp (+ (* (+ (ddsketch-min-key proto) b -1) (ddsketch-log-gamma proto))
                             log-mid))))))))

(defvar *sketch-reps* (make-hash-table :test 'equal))
(defvar *sketch-reps-lock* (bt:make-lock "prometheus ddsketch bucket values"))

(defun %sketch-reps (proto)
  "Bucket values for PROTO's parameters, built once and shared."
  (let ((key (list (ddsketch-relative-error proto) (ddsketch-min-value proto)
                   (ddsketch-max-value proto))))
    (bt:with-lock-held (*sketch-reps-lock*)
      (or (gethash key *sketch-reps*)
          (setf (gethash key *sketch-reps*) (%build-sketch-reps proto))))))

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

(declaim (inline %observed-span))
(defun %observed-span (sketch min max)
  "First and last bucket that can hold a value in [MIN, MAX], widened by one
on each side against rounding in LOG. FIRST > LAST when nothing was observed:
MIN is then the largest double and MAX 0."
  (declare (type ddsketch sketch) (type double-float min max))
  (let ((last (1- (ddsketch-buckets sketch))))
    (if (> min max)
        (values 1 0)
        (values (max 0 (1- (%sketch-index sketch (max 0d0 min))))
                (min last (1+ (%sketch-index sketch (max 0d0 max))))))))

;; inline: count and sum come back as doubles, a full call would box them
(declaim (inline %quantiles-from-data))
(defun %quantiles-from-data (proto data qs out
                             &optional (from 0) (to (1- (ddsketch-buckets proto))))
  "Write into OUT the quantiles QS (ascending) of DATA, an array laid out like
PROTO's, in one pass over its counts and without allocating. Only buckets
FROM..TO are read: the caller knows the rest are empty. Returns whether
anything was observed (OUT is left alone otherwise), the count as a double
and the sum, all from the same pass over DATA."
  (declare (type ddsketch proto) (type sketch-data data) (type quantile-buffer qs out)
           (type fixnum from to) (optimize (speed 3)))
  (let* ((m (%pair-word (ddsketch-base proto)))
         (first (+ m 2))
         (total 0d0)
         (sum 0d0)
         (lowest -1)
         (highest -1))
    (declare (type fixnum m first lowest highest) (type double-float total sum))
    ;; counts before extrema: an observation's extrema are written before
    ;; its count, so counted observations always have their extrema in
    (loop for b of-type fixnum from from to to
          do (let ((c (aref data (+ first (* 2 b)))))
               (unless (zerop c)
                 (when (< lowest 0) (setf lowest b))
                 (setf highest b))
               (incf total c)
               (incf sum (aref data (+ first (* 2 b) 1)))))
    (when (zerop total)
      (return-from %quantiles-from-data (values nil 0d0 0d0)))
    ;; extrema are written before counts, so they may come from an
    ;; observation this read did not count. Bounding them by the value of the
    ;; lowest and highest counted bucket keeps them within alpha of what was
    ;; counted. Underflow and overflow buckets carry no bound.
    (let* ((reps (ddsketch-reps proto))
           (last (1- (ddsketch-buckets proto)))
           (min (let ((min (aref data m)))
                  (if (= lowest 0) min (max min (aref reps lowest)))))
           (max (let ((max (aref data (1+ m))))
                  (if (= highest last) max (min max (aref reps highest)))))
           (n (length qs))
          (qi 0)
          (seen 0d0)
          (last-rank (- total 1d0)))
      (declare (type sketch-data reps) (type fixnum last) (type double-float min max seen last-rank)
               (type fixnum n qi))
      ;; crossed: one bucket, with everything live on one side of its value.
      ;; The live extremum on that side is within alpha.
      (when (> min max)
        (if (= min (aref data m)) (setf max min) (setf min max)))
      (loop while (and (< qi n) (<= (aref qs qi) 0d0))
            do (setf (aref out qi) min) (incf qi))
      (loop for bucket of-type fixnum from from to to do
        (when (or (= qi n) (>= (aref qs qi) 1d0))
          (return))
        (incf seen (aref data (+ first (* 2 bucket))))
        (loop while (and (< qi n)
                         (< (aref qs qi) 1d0)
                         (> seen (* (aref qs qi) last-rank)))
              do (let ((estimate (aref reps bucket)))
                   (setf (aref out qi) (cond ((< estimate min) min)
                                             ((> estimate max) max)
                                             (t estimate))))
                 (incf qi)))
      ;; q = 1, or counts grew during the walk
      (loop while (< qi n)
            do (setf (aref out qi) max) (incf qi))
      (values t total sum))))

(declaim (inline %sketch-quantiles-into))
(defun %sketch-quantiles-into (sketch qs out)
  ;; extrema first, then the buckets they span
  (multiple-value-bind (lo hi) (%observed-span sketch (%sketch-min sketch) (%sketch-max sketch))
    (and (<= lo hi)
         (%quantiles-from-data sketch (ddsketch-data sketch) qs out lo hi))))

(defun ddsketch-quantiles-into (sketch qs out)
  "Write the quantiles QS (an ascending (SIMPLE-ARRAY DOUBLE-FLOAT (*)) in
[0, 1]) of SKETCH into OUT, a double-float array of the same length. Returns
NIL and leaves OUT alone when SKETCH is empty. Allocates nothing."
  (check-quantile-buffers qs out)
  ;; first value only: count and sum would come back boxed
  (values (%sketch-quantiles-into sketch qs out)))

(defun ddsketch-quantile (sketch q)
  "Estimate the Q-quantile, 0 <= Q <= 1. Returns NIL for an empty sketch.
Within the relative error for q = 0 and q = 1 too, not the exact extremum:
use DDSKETCH-MIN and DDSKETCH-MAX for those."
  (unless (and (realp q) (<= 0 q 1))
    (error 'invalid-value-error :value q :reason "quantile should be in [0, 1]"))
  (let ((qs (make-array 1 :element-type 'double-float :initial-element (coerce q 'double-float)))
        (out (make-array 1 :element-type 'double-float :initial-element 0d0)))
    (declare (dynamic-extent qs out))
    (when (%sketch-quantiles-into sketch qs out)
      (aref out 0))))

(declaim (inline %read-pair))
(defun %read-pair (sketch pair)
  "Pair PAIR of SKETCH read in one step, so a concurrent update is either
fully in or fully out."
  (declare (type ddsketch sketch) (type fixnum pair))
  (let ((data (ddsketch-data sketch)))
    #+prometheus-cas-pair
    (if **atomic-pair-load**
        (%load-pair data pair)
        ;; without AVX: a CAS expecting (0 0) and writing (0 0) returns the
        ;; pair atomically and changes nothing, but takes the line
        (multiple-value-bind (a b) (%cas-pair data pair 0 0 0 0)
          (values (%w->d a) (%w->d b))))
    #-prometheus-cas-pair
    (%with-sketch-locked (sketch)
      (values (aref data (%pair-word pair)) (aref data (1+ (%pair-word pair)))))))

(defun ddsketch-merge (into from)
  "Add every observation of FROM to INTO. Both must have been made with the
same parameters. Safe while either is being observed into: each bucket of
FROM is read in one step, though an observation still being written into
FROM may be left out. Allocates nothing."
  (unless (ddsketch-compatible-p into from)
    (error 'invalid-value-error :value from
                                :reason "sketches were created with different parameters"))
  ;; extrema copied below may cover observations not merged yet. Reads
  ;; bound them by the buckets they see.
  (let ((base (ddsketch-base from)))
    (dotimes (b (ddsketch-buckets from))
      ;; most buckets are empty: skip those before the locked read
      (unless (zerop (%bucket-count from b))
        (multiple-value-bind (c s) (%read-pair from (+ base 1 b))
          ;; extrema read after the bucket: observe writes them first, so
          ;; they cover it. Min only falls and max only rises, so reading
          ;; the two without a CAS is fine.
          (%sketch-add into b c s (%sketch-min from) (%sketch-max from))))))
  into)

(defun check-merge-target (into sources)
  "Signal INVALID-VALUE-ERROR before INTO is touched when it cannot take the
sketches in SOURCES (a simple-vector): different parameters, or one of them,
which resetting it would wipe."
  (declare (type simple-vector sources))
  (loop for from across sources
        do (when (eq into from)
             (error 'invalid-value-error :value into
                                         :reason "merge target is one of the sketches merged"))
           (unless (ddsketch-compatible-p into from)
             (error 'invalid-value-error :value into
                                         :reason "sketches were created with different parameters"))))

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

;;; One sketch per CPU, combined on read. Avoids cache-line contention on
;;; buckets and extremes. Reads merge the shards into one buffer, owned by
;;; the sketch and guarded by a lock only readers take.

(defstruct (sharded-ddsketch (:constructor %make-sharded-ddsketch)
                             (:copier nil))
  (shards #() :type simple-vector :read-only t)
  (merged (make-array 0 :element-type 'double-float) :type sketch-data :read-only t)
  (lock (bt:make-lock "prometheus sharded ddsketch read lock") :read-only t))

(defun make-sharded-ddsketch (&key (shards (default-shards)) (relative-error 0.01d0) (min-value 1d-9) (max-value 1d9))
  "Like MAKE-DDSKETCH, spread over SHARDS (a power of two) sub-sketches, all
allocated now with one more buffer for reads. Only SBCL shards; elsewhere one
locked sketch is used."
  (check-ddsketch-parameters relative-error min-value max-value shards)
  (let ((vector (make-array #+sbcl shards #-sbcl 1)))
    (dotimes (i (length vector))
      (setf (svref vector i) (make-ddsketch :relative-error relative-error
                                            :min-value min-value :max-value max-value)))
    (%make-sharded-ddsketch
     :shards vector
     :merged (make-array (length (ddsketch-data (svref vector 0)))
                         :element-type 'double-float :initial-element 0d0))))

(defun %merge-shards-into (shards data)
  "Set DATA, laid out like the shards, to the sum of SHARDS. Each source pair
is read in one step. DATA must not be written concurrently. Returns DATA and
the first and last bucket that can be non-zero.

Per shard only the buckets between its observed min and max are read: every
counted value lies within them, since observe writes the extrema first and
min only falls and max only rises. Buckets outside are exactly zero."
  (declare (type simple-vector shards) (type sketch-data data) (optimize (speed 3)))
  (let* ((proto (the ddsketch (svref shards 0)))
         (buckets (ddsketch-buckets proto))
         (base (ddsketch-base proto))
         (m (%pair-word base))
         (first-pair (1+ base))
         (from buckets)
         (to -1))
    (declare (type fixnum buckets base m first-pair from to))
    (fill data 0d0 :start (+ m 2) :end (+ m 2 (* 2 buckets)))
    (loop for s across shards
          do (let ((shard s))
               (declare (type ddsketch shard))
               ;; extrema first: anything counted before this read is inside
               (multiple-value-bind (lo hi)
                   (%observed-span shard (%sketch-min shard) (%sketch-max shard))
                 (declare (type fixnum lo hi))
                 (when (<= lo hi)
                   (setf from (min from lo) to (max to hi))
                   (let ((src (ddsketch-data shard)))
                     #+prometheus-cas-pair
                     (if **atomic-pair-load**
                         ;; same instructions for every bucket in the span
                         (loop for p of-type fixnum from (+ first-pair lo) to (+ first-pair hi)
                               do (%pair-add data p src p))
                         (loop for b of-type fixnum from lo to hi
                               unless (zerop (%bucket-count shard b))
                                 do (multiple-value-bind (c sum) (%read-pair shard (+ first-pair b))
                                      (let ((w (%pair-word (+ first-pair b))))
                                        (incf (aref data w) c)
                                        (incf (aref data (1+ w)) sum)))))
                     #-prometheus-cas-pair
                     (%with-sketch-locked (shard)
                       (loop for w of-type fixnum from (%pair-word (+ first-pair lo))
                               below (%pair-word (+ first-pair hi 1))
                             do (incf (aref data w) (aref src w)))))))))
    ;; extrema again after the buckets, so they cover every count summed
    (let ((min most-positive-double-float) (max 0d0))
      (declare (type double-float min max))
      (loop for s across shards
            do (let ((shard s))
                 (declare (type ddsketch shard))
                 (setf min (min min (%sketch-min shard))
                       max (max max (%sketch-max shard)))))
      (setf (aref data m) min
            (aref data (1+ m)) max))
    (values data from to)))

(defun sharded-ddsketch-observe (sketch value)
  "Record VALUE in the current CPU's shard. Allocates nothing."
  (let ((shards (sharded-ddsketch-shards sketch)))
    (ddsketch-observe (svref shards (%current-shard-index (1- (length shards)))) value))
  sketch)

(defmacro %with-sharded-read ((observed count sum) (sketch qs out) &body body)
  "Merge SKETCH's shards and walk them for QS into OUT, then run BODY with
OBSERVED, COUNT (a double) and SUM bound, all from that one read, inside the
read lock. Values never leave the locked region, so nothing is boxed."
  (let ((s (gensym "SKETCH")) (shards (gensym "SHARDS")) (data (gensym "DATA"))
        (from (gensym "FROM")) (to (gensym "TO")))
    `(let ((,s ,sketch))
       (bt:with-lock-held ((sharded-ddsketch-lock ,s))
         (let ((,shards (sharded-ddsketch-shards ,s)))
           (multiple-value-bind (,data ,from ,to)
               (%merge-shards-into ,shards (sharded-ddsketch-merged ,s))
             (declare (type fixnum ,from ,to))
             (multiple-value-bind (,observed ,count ,sum)
                 (if (<= ,from ,to)
                     (%quantiles-from-data (svref ,shards 0) ,data ,qs ,out ,from ,to)
                     (values nil 0d0 0d0))
               (declare (ignorable ,observed ,count ,sum))
               ,@body)))))))

(defun sharded-ddsketch-quantiles-into (sketch qs out)
  "DDSKETCH-QUANTILES-INTO over all shards, merged into the sketch's buffer."
  (check-quantile-buffers qs out)
  (%with-sharded-read (observed count sum) (sketch qs out)
    observed))

(defun sharded-ddsketch-quantile (sketch q)
  (unless (and (realp q) (<= 0 q 1))
    (error 'invalid-value-error :value q :reason "quantile should be in [0, 1]"))
  (let ((qs (make-array 1 :element-type 'double-float :initial-element (coerce q 'double-float)))
        (out (make-array 1 :element-type 'double-float :initial-element 0d0)))
    (declare (dynamic-extent qs out))
    (%with-sharded-read (observed count sum) (sketch qs out)
      (when observed (aref out 0)))))

(defun sharded-ddsketch-count (sketch)
  (%count->integer
   (loop for shard across (sharded-ddsketch-shards sketch)
         sum (%sketch-total shard) of-type double-float)))

(defun sharded-ddsketch-sum (sketch)
  (loop for shard across (sharded-ddsketch-shards sketch)
        sum (ddsketch-sum shard) of-type double-float))

(defun sharded-ddsketch-snapshot (sketch &optional into)
  "A DDSKETCH holding every observation made so far, all shards merged. With
INTO (a compatible sketch, not observed into meanwhile) it is reused instead
of allocating."
  (let* ((shards (sharded-ddsketch-shards sketch))
         (result (or into (ddsketch-empty-copy (svref shards 0)))))
    (check-merge-target result shards)
    (if (%same-layout-p result (svref shards 0))
        (%merge-shards-into shards (ddsketch-data result))
        (progn (ddsketch-reset result)
               (loop for shard across shards do (ddsketch-merge result shard))))
    result))
