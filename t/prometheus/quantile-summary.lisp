(in-package :prometheus.test)

(plan #+sbcl 6 #-sbcl 5)

(defun within-relative-error-p (estimate exact alpha)
  (<= (abs (- estimate exact)) (* alpha exact (+ 1 1d-9))))

(defun exact-quantile (sorted q)
  ;; lower rank-q*(n-1) element, the definition the sketch targets
  (aref sorted (floor (* q (1- (length sorted))))))

(subtest "DDSketch"
  (subtest "Errors & Validations"
    (is-error-report (prom:make-ddsketch :relative-error 0) prom:invalid-value-error
                     "Value 0 is invalid. Reason: relative error should be in (0, 1)")
    (is-error-report (prom:make-ddsketch :min-value 10 :max-value 1) prom:invalid-value-error
                     "Value (10 . 1) is invalid. Reason: expected 1d-300 < min-value < max-value < 1d300")
    (is-error-report (prom:ddsketch-observe (prom:make-ddsketch) -1) prom:invalid-value-error
                     "Value -1 is invalid. Reason: value should be a finite non-negative real number")
    (is-error-report (prom:ddsketch-quantile (prom:make-ddsketch) 2) prom:invalid-value-error
                     "Value 2 is invalid. Reason: quantile should be in [0, 1]"))

  (subtest "Parameter limits"
    (is-error (prom:make-ddsketch :relative-error 1d-20) prom:invalid-value-error
              "gamma rounds to 1")
    (is-error (prom:make-ddsketch :relative-error 1d-15 :min-value 1d0 :max-value 1.000000000001d0)
              prom:invalid-value-error
              "below what rounding in LOG allows")
    (is-error (prom:make-ddsketch :relative-error 1d-6) prom:invalid-value-error
              "too many buckets")
    (is-error (prom:make-ddsketch :relative-error (- 1 (expt 10 -20))) prom:invalid-value-error
              "alpha rounds to 1")
    ;; gamma near 2^40: gamma^key alone overflows for the top bucket
    (let* ((s (prom:make-ddsketch :relative-error (- 1d0 (expt 2d0 -40)) :min-value 1d-299 :max-value 9d299))
           (top (aref (prom::ddsketch-reps s) (- (prom::ddsketch-buckets s) 2))))
      (ok (< 0d0 top most-positive-double-float) "top bucket value is finite"))
    ;; few buckets but keys far from zero: observing must not hit a type error
    (let ((s (prom:make-ddsketch :relative-error 1d-6 :min-value 1d-9 :max-value 1.00001d-9)))
      (prom:ddsketch-observe s 1d-9)
      (prom:ddsketch-observe s 1.000005d-9)
      (is (prom:ddsketch-count s) 2)))

  (subtest "Extrema stay within the counted buckets"
    ;; as if two more observations wrote their extrema but not yet their counts
    (let* ((s (prom:make-ddsketch))
           (data (prom::ddsketch-data s))
           (m (* 2 (prom::ddsketch-base s))))
      (prom:ddsketch-observe s 100d0)
      (prom:ddsketch-observe s 200d0)
      (setf (aref data m) 1d-6
            (aref data (1+ m)) 1d6)
      (ok (within-relative-error-p (prom:ddsketch-quantile s 0) 100d0 0.01d0) "q=0")
      (ok (within-relative-error-p (prom:ddsketch-quantile s 1) 200d0 0.01d0) "q=1"))
    ;; one bucket below and above: the counted bucket's value, within alpha
    (let* ((s (prom:make-ddsketch))
           (data (prom::ddsketch-data s))
           (m (* 2 (prom::ddsketch-base s))))
      (prom:ddsketch-observe s 1d0)
      (setf (aref data m) 0.979d0
            (aref data (1+ m)) 1.021d0)
      (ok (within-relative-error-p (prom:ddsketch-quantile s 0) 1d0 0.01d0) "q=0, one bucket below")
      (ok (within-relative-error-p (prom:ddsketch-quantile s 1) 1d0 0.01d0) "q=1, one bucket above"))
    ;; same bucket: 1.0001 counted, 1.019 only in max
    (let* ((s (prom:make-ddsketch))
           (data (prom::ddsketch-data s))
           (m (* 2 (prom::ddsketch-base s))))
      (prom:ddsketch-observe s 1.0001d0)
      (setf (aref data (1+ m)) 1.019d0)
      (ok (within-relative-error-p (prom:ddsketch-quantile s 1) 1.0001d0 0.01d0) "q=1, same bucket"))
    ;; a quiet sketch keeps extrema already inside the bound
    (let ((s (prom:make-ddsketch)))
      (prom:ddsketch-observe s 0.995d0)
      (prom:ddsketch-observe s 1.005d0)
      (is (prom:ddsketch-quantile s 0) 0.995d0)
      (is (prom:ddsketch-quantile s 1) 1.005d0)))

  (subtest "Bucket values are shared"
    (ok (eq (prom::ddsketch-reps (prom:make-ddsketch)) (prom::ddsketch-reps (prom:make-ddsketch)))))

  (subtest "Empty"
    (let ((s (prom:make-ddsketch)))
      (is (prom:ddsketch-count s) 0)
      (is (prom:ddsketch-quantile s 0.5) nil)))

  (subtest "Relative error bound"
    (let* ((alpha 0.01d0)
           (s (prom:make-ddsketch :relative-error alpha))
           (values (loop for i from 1 to 5000
                         ;; spread over 6 orders of magnitude
                         collect (* 1d-3 (expt 1.003d0 i))))
           (sorted (sort (coerce values 'vector) #'<)))
      (dolist (v values) (prom:ddsketch-observe s v))
      (is (prom:ddsketch-count s) 5000)
      (ok (< (abs (- (prom:ddsketch-sum s) (reduce #'+ values))) 1d-6))
      (is (prom:ddsketch-quantile s 0) (aref sorted 0))
      (is (prom:ddsketch-quantile s 1) (aref sorted (1- (length sorted))))
      (dolist (q '(0.01d0 0.25d0 0.5d0 0.9d0 0.99d0 0.999d0))
        (ok (within-relative-error-p (prom:ddsketch-quantile s q) (exact-quantile sorted q) alpha)
            (format nil "q=~a within ~a" q alpha)))))

  (subtest "Out of range values"
    (let ((s (prom:make-ddsketch :min-value 1 :max-value 100)))
      (prom:ddsketch-observe s 0)
      (prom:ddsketch-observe s 0.5d0)
      (prom:ddsketch-observe s 1000)
      (is (prom:ddsketch-count s) 3)
      (is (prom:ddsketch-quantile s 0) 0d0)
      (is (prom:ddsketch-quantile s 1) 1000d0)
      ;; underflow reports min-value, clamped to what was seen
      (is (prom:ddsketch-quantile s 0.5d0) 1d0)))

  (subtest "Merge"
    (let* ((a (prom:make-ddsketch))
           (b (prom:make-ddsketch))
           (whole (prom:make-ddsketch)))
      (loop for i from 1 to 1000
            do (prom:ddsketch-observe a i)
               (prom:ddsketch-observe whole i))
      (loop for i from 1 to 1000
            do (prom:ddsketch-observe b (* 1000 i))
               (prom:ddsketch-observe whole (* 1000 i)))
      (let ((merged (prom:ddsketch-merged a b)))
        (is (prom:ddsketch-count merged) 2000)
        (is (prom:ddsketch-sum merged) (prom:ddsketch-sum whole))
        (is (prom:ddsketch-min merged) 1d0)
        (is (prom:ddsketch-max merged) 1d6)
        (dolist (q '(0.1d0 0.5d0 0.9d0 0.99d0))
          (is (prom:ddsketch-quantile merged q) (prom:ddsketch-quantile whole q)
              (format nil "merged q=~a equals single sketch" q))))
      ;; inputs untouched
      (is (prom:ddsketch-count a) 1000))
    (is-error (prom:ddsketch-merge (prom:make-ddsketch) (prom:make-ddsketch :relative-error 0.02d0))
              prom:invalid-value-error)
    ;; same gamma and first key, different underflow meaning
    (is-error (prom:ddsketch-merge (prom:make-ddsketch :min-value 1.00001d0 :max-value 100)
                                   (prom:make-ddsketch :min-value 1.02d0 :max-value 100))
              prom:invalid-value-error))

  (subtest "Concurrent observe"
    (let ((s (prom:make-ddsketch)))
      (mapc #'bt:join-thread
            (loop repeat 8
                  collect (bt:make-thread
                           (lambda ()
                             (loop for i from 1 to 10000 do (prom:ddsketch-observe s i))))))
      (is (prom:ddsketch-count s) 80000)
      (is (prom:ddsketch-sum s) (coerce (* 8 (/ (* 10000 10001) 2)) 'double-float))
      (is (prom:ddsketch-min s) 1d0)
      (is (prom:ddsketch-max s) 10000d0))))

(subtest "Sharded DDSketch"
  (is-error (prom:make-sharded-ddsketch :shards 3) prom:invalid-value-error)
  ;; cgroup quotas, v2 "max" and v1 -1 mean none
  (is (prom::%quota-cpus "150000" "100000") 2)
  (is (prom::%quota-cpus "max" "100000") nil)
  (is (prom::%quota-cpus "-1" "100000") nil)
  (ok (<= 1 (prom::detect-cpu-count)) "cpu count")
  (is (prom::%cgroup-dirs "/sys/fs/cgroup" "/a/b") '("/sys/fs/cgroup/a/b" "/sys/fs/cgroup/a" "/sys/fs/cgroup")
      :test #'equal)
  (is (prom::%cgroup-dirs "/sys/fs/cgroup" "/") '("/sys/fs/cgroup") :test #'equal)
  ;; sparse affinity spreads over dense slots
  (let ((slots (prom::make-cpu-slots '(0 4 8 12))))
    (is (loop for cpu in '(0 4 8 12) collect (logand (aref slots cpu) 3)) '(0 1 2 3) :test #'equal))
  (let ((s (prom:make-sharded-ddsketch :shards 16))
        (reference (prom:make-ddsketch)))
    (is (prom:sharded-ddsketch-count s) 0)
    (loop for i from 1 to 10000 do (prom:ddsketch-observe reference i))
    (mapc #'bt:join-thread
          (loop repeat 8
                collect (bt:make-thread
                         (lambda ()
                           (loop for i from 1 to 10000 do (prom:sharded-ddsketch-observe s i))))))
    (is (prom:sharded-ddsketch-count s) 80000)
    (is (prom:sharded-ddsketch-sum s) (coerce (* 8 (/ (* 10000 10001) 2)) 'double-float))
    (let ((snapshot (prom:sharded-ddsketch-snapshot s)))
      (is (prom:ddsketch-count snapshot) 80000)
      (is (prom:ddsketch-min snapshot) 1d0)
      (is (prom:ddsketch-max snapshot) 10000d0)
      ;; 8 copies of the data have the quantiles of one copy
      (dolist (q '(0 0.1d0 0.5d0 0.99d0 1))
        (is (prom:ddsketch-quantile snapshot q) (prom:ddsketch-quantile reference q)
            (format nil "snapshot q=~a" q))
        (is (prom:sharded-ddsketch-quantile s q) (prom:ddsketch-quantile reference q)
            (format nil "in place q=~a" q))))))

#+sbcl
(subtest "Nothing allocated on observe and read paths"
  ;; Bytes consed is global and per region: repeat the work, take the
  ;; minimum of a few runs so other threads cannot fail the probe.
  (flet ((consed (thunk)
           (funcall thunk)
           (loop repeat 5
                 minimize (let ((before (sb-ext:get-bytes-consed)))
                            (funcall thunk)
                            (- (sb-ext:get-bytes-consed) before)))))
    (let* ((sketch (prom:make-ddsketch))
           (sharded (prom:make-sharded-ddsketch))
           (target (prom:make-ddsketch))
           (qs (make-array 3 :element-type 'double-float :initial-contents '(0.5d0 0.9d0 0.99d0)))
           (out (make-array 3 :element-type 'double-float :initial-element 0d0))
           (summary (prom:make-quantile-summary :name "qwe" :help "" :registry nil))
           ;; boxed once here, so the loops below pass existing objects
           (values (coerce (loop for i from 1 to 1000 collect (* i 1d-3)) 'simple-vector))
           (metric (prom:get-metric summary nil)))
      (is (consed (lambda () (dotimes (r 100) (loop for v across values do (prom:ddsketch-observe sketch v))))) 0
          "ddsketch-observe")
      (is (consed (lambda () (dotimes (r 100) (loop for i from 1 to 1000 do (prom:sharded-ddsketch-observe sharded i))))) 0
          "sharded-ddsketch-observe")
      (is (consed (lambda () (dotimes (r 100) (loop for v across values do (prom:summary.observe metric v))))) 0
          "summary.observe on a series")
      (is (consed (lambda () (dotimes (r 5000) (prom:sharded-ddsketch-quantiles-into sharded qs out)))) 0
          "sharded-ddsketch-quantiles-into")
      (is (consed (lambda () (dotimes (r 5000) (prom:with-quantile-summary-estimates (q e) metric (declare (type (simple-array double-float (*)) e)) (> (aref e 0) (aref q 0)))))) 0
          "with-quantile-summary-estimates")
      (is (consed (lambda () (dotimes (r 5000) (prom:sharded-ddsketch-snapshot sharded target)))) 0
          "snapshot into a reused sketch"))))

(subtest "Span-limited reads match a full merge"
  ;; four standalone sketches, so this runs where only one shard is made
  (let ((shards (vector (prom:make-ddsketch) (prom:make-ddsketch)
                        (prom:make-ddsketch) (prom:make-ddsketch))))
    ;; different, partly disjoint ranges; the last stays empty
    (loop for v from 1 to 2000 do (prom:ddsketch-observe (svref shards 0) (* v 1d-3)))
    (loop for v from 1 to 2000 do (prom:ddsketch-observe (svref shards 1) (* v 7d0)))
    (loop for v from 1 to 500 do (prom:ddsketch-observe (svref shards 2) (expt 1.01d0 v)))
    (prom:ddsketch-observe (svref shards 2) 0)
    (prom:ddsketch-observe (svref shards 2) 1d12)
    (let ((full (prom:ddsketch-empty-copy (svref shards 0)))
          (span (prom:ddsketch-empty-copy (svref shards 0)))
          (qs (make-array 7 :element-type 'double-float
                            :initial-contents '(0d0 0.001d0 0.1d0 0.5d0 0.9d0 0.999d0 1d0)))
          (span-out (make-array 7 :element-type 'double-float))
          (full-out (make-array 7 :element-type 'double-float)))
      ;; ddsketch-merge walks every bucket
      (loop for shard across shards do (prom:ddsketch-merge full shard))
      (multiple-value-bind (data from to) (prom::%merge-shards-into shards (prom::ddsketch-data span))
        (declare (ignore data))
        (is (prom:ddsketch-count span) (prom:ddsketch-count full))
        (is (prom:ddsketch-sum span) (prom:ddsketch-sum full))
        (ok (equalp (prom::ddsketch-data span) (prom::ddsketch-data full)) "same buckets")
        (prom::%quantiles-from-data span (prom::ddsketch-data span) qs span-out from to)
        (prom:ddsketch-quantiles-into full qs full-out)
        (is span-out full-out "same quantiles" :test #'equalp)))))

(subtest "Consistency under concurrency"
  (subtest "A counted observation has its extrema"
    (let ((bad 0))
      (dotimes (i 300)
        (let* ((s (prom:make-ddsketch))
               (writer (bt:make-thread (lambda () (prom:ddsketch-observe s 5d0)))))
          (loop for alive = (bt:thread-alive-p writer)
                for q = (prom:ddsketch-quantile s 0.5d0)
                when (and q (/= q 5d0)) do (incf bad)
                while alive)
          (bt:join-thread writer)))
      (is bad 0)))

  (subtest "One read gives count, sum and quantiles of the same observations"
    (let* ((summary (prom:make-quantile-summary :name "qwe" :help "" :quantiles '(0.5) :registry nil))
           (m (prom:get-metric summary nil))
           (stop nil)
           (writers (loop repeat 4
                          collect (bt:make-thread (lambda () (loop until stop do (prom:summary.observe m 2d0))))))
           (bad 0))
      (unwind-protect
           (dotimes (i 200)
             (prom:with-quantile-summary-read (qs estimates count sum) m
               (declare (ignore qs) (type unsigned-byte count))
               (unless (and (= sum (* 2d0 count))
                            (or (zerop count) (= (aref estimates 0) 2d0)))
                 (incf bad))))
        (setf stop t)
        (mapc #'bt:join-thread writers))
      (is bad 0)))

  (subtest "A snapshot never splits count from sum"
    (let* ((s (prom:make-sharded-ddsketch))
           (stop nil)
           (writers (loop repeat 4
                          collect (bt:make-thread (lambda () (loop until stop do (prom:sharded-ddsketch-observe s 2d0))))))
           (target (prom:ddsketch-empty-copy (svref (prom::sharded-ddsketch-shards s) 0)))
           (bad 0))
      (unwind-protect
           (dotimes (i 200)
             (prom:sharded-ddsketch-snapshot s target)
             (unless (= (prom:ddsketch-sum target) (* 2d0 (prom:ddsketch-count target)))
               (incf bad)))
        (setf stop t)
        (mapc #'bt:join-thread writers))
      (is bad 0)))

  (subtest "A snapshot into a sketch with another layout"
    (let* ((s (prom:make-sharded-ddsketch))
           (target (let ((prom::*cache-line-bytes* (* 4 prom::*cache-line-bytes*)))
                     (prom:make-ddsketch))))
      (loop for v from 1 to 1000 do (prom:sharded-ddsketch-observe s v))
      (prom:sharded-ddsketch-snapshot s target)
      (is (prom:ddsketch-count target) 1000)
      (is (prom:ddsketch-sum target) 500500d0)
      (is (prom:ddsketch-quantile target 1) 1000d0))))

(subtest "Quantile Summary"
  (subtest "Errors & Validations"
    (is-error-report (prom:make-quantile-summary :name "qwe" :help "" :labels '("quantile") :registry nil)
                     prom:invalid-label-name-error
                     "Label name \"quantile\" is invalid. Reason: summary cannot have a label named \"quantile\"")
    (is-error-report (prom:make-quantile-summary :name "qwe" :help "" :quantiles '(1.5) :registry nil)
                     prom:invalid-value-error
                     "Value 1.5 is invalid. Reason: quantile should be in [0, 1]")
    (is-error (prom:make-quantile-summary :name "qwe" :help "" :labels '("Quantile") :registry nil)
              prom:invalid-label-name-error)
    (is-error (prom:summary.observe (prom:make-quantile-summary :name "qwe" :help "" :registry nil) 12 :count 2)
              prom:invalid-value-error)
    (with-fresh-registry
      (is-error (prom:make-quantile-summary :name "qwe" :help "" :value -1) prom:invalid-value-error)
      (ok (prom:make-quantile-summary :name "qwe" :help "" :value 1) "name not taken by the failed attempt"))
    (is-error-report (prom:summary.observe (prom:make-quantile-summary :name "qwe" :help "" :registry nil) -1)
                     prom:invalid-value-error
                     "Value -1 is invalid. Reason: value should be a finite non-negative real number"))

  (subtest "OBSERVE"
    (let* ((s (prom:make-quantile-summary :name "qwe" :help "" :registry nil))
           (m (prom:get-metric s nil)))
      (is (prom:summary-quantiles m) nil)
      (loop for i from 1 to 100 do (prom:summary.observe s i))
      (is (prom:summary-count m) 100)
      (is (prom:summary-sum m) 5050d0)
      (is (mapcar #'car (prom:summary-quantiles m)) '(0.5d0 0.9d0 0.95d0))
      (ok (within-relative-error-p (cdr (assoc 0.5d0 (prom:summary-quantiles m))) 50 0.01d0))))

  (subtest "Single-float quantiles become the double the caller wrote"
    (let ((s (prom:make-quantile-summary :name "qwe" :help "" :quantiles '(0.95 0.99) :registry nil)))
      (is (coerce (prom::quantile-summary-quantiles s) 'list) '(0.95d0 0.99d0))))

  (subtest "Merged sketch across labels"
    (let ((s (prom:make-quantile-summary :name "qwe" :help "" :labels '("path") :registry nil)))
      (loop for i from 1 to 100 do (prom:summary.observe s i :labels '("/a")))
      (loop for i from 101 to 200 do (prom:summary.observe s i :labels '("/b")))
      (is (prom:ddsketch-count (prom:quantile-summary-merged-sketch s)) 200)
      (is (prom:ddsketch-count (prom:quantile-summary-merged-sketch
                                s :test (lambda (labels) (equal labels '("/a")))))
          100)
      (is (prom:quantile-summary-merged-sketch s :test (constantly nil)) nil)))

  (subtest "REGISTRY"
    (with-fresh-registry
      (let ((s (prom:make-quantile-summary :name "qwe" :help "" :value 12)))
        (is (prom:registeredp s prom:*default-registry*) s)))))

(finalize)
