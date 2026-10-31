(in-package :prometheus.test)

(plan 4)

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
      (is (consed (lambda () (dotimes (r 50) (prom:sharded-ddsketch-quantiles-into sharded qs out)))) 0
          "sharded-ddsketch-quantiles-into")
      (is (consed (lambda () (dotimes (r 50) (prom:with-quantile-summary-estimates (q e) metric (aref e 0) (aref q 0))))) 0
          "with-quantile-summary-estimates")
      (is (consed (lambda () (dotimes (r 50) (prom:sharded-ddsketch-snapshot sharded target)))) 0
          "snapshot into a reused sketch"))))

(subtest "Quantile Summary"
  (subtest "Errors & Validations"
    (is-error-report (prom:make-quantile-summary :name "qwe" :help "" :labels '("quantile") :registry nil)
                     prom:invalid-label-name-error
                     "Label name \"quantile\" is invalid. Reason: summary cannot have a label named \"quantile\"")
    (is-error-report (prom:make-quantile-summary :name "qwe" :help "" :quantiles '(1.5) :registry nil)
                     prom:invalid-value-error
                     "Value 1.5 is invalid. Reason: quantile should be in [0, 1]")
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
