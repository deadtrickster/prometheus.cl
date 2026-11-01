(in-package :prometheus.test)

(plan 3)

(subtest "Counter"
  (subtest "Errors & Validations"
    (is-error-report (prom:counter.inc nil :value "qwe") prom:invalid-value-error "Value \"qwe\" is invalid. Reason: value is not a number")
    (is-error-report (prom:counter.inc nil :value -1) prom:invalid-value-error "Value -1 is invalid. Reason: counters can only be incremented by non-negative amounts")
    (is-error-report (prom:make-counter :name "qwe" :help "" :value -1 :registry nil) prom:invalid-value-error "Value -1 is invalid. Reason: counters can only be incremented by non-negative amounts")
    (is-error-report (prom:make-counter :name "qwe" :help "" :value 12 :labels '("qwe") :registry nil) prom:invalid-value-error "Value 12 is invalid. Reason: can only specify at most one of value and labels"))

  (subtest "INC & RESET"
    (subtest "No labels"
      (let* ((c (prom:make-counter :name "qwe" :help "" :value 12 :registry nil)))
        (prom:counter.inc c :value 2)
        (prom:counter.inc c)
        (is (prom:metric-value c) 15)
        (prom:counter.reset c)
        (is (prom:metric-value c) prom::+counter-default+)))
    (subtest "With labels"
      (let* ((c (prom:make-counter :name "qwe" :help "" :labels '("method") :registry nil)))
        (prom:counter.inc c :value 2 :labels '("get"))
        (prom:counter.inc c :labels '("get"))
        (is (prom:metric-value (prom:get-metric c '("get"))) 3)
        (prom:counter.reset c :labels '("get"))
        (is (prom:metric-value (prom:get-metric c '("get"))) prom::+counter-default+))))

  (subtest "REGISTRY"
    (with-fresh-registry
      (let ((c (prom:make-counter :name "qwe" :help "" :value 12)))
        (is (prom:registeredp c prom:*default-registry*) c)))))

(subtest "Sharded counter"
  (is-error (prom:make-counter :name "qwe" :help "" :shards 3 :registry nil) prom:invalid-value-error)
  (ok (= 1 (logcount (prom:default-shards))) "default shards is a power of two")
  (let ((prom:*default-shards* 4))
    (is (prom::counter-shards (prom:make-counter :name "qwe" :help "" :registry nil)) 4))

  (subtest "Concurrent inc is exact"
    (let* ((c (prom:make-counter :name "qwe" :help "" :registry nil))
           (m (prom:get-metric c nil)))
      (mapc #'bt:join-thread
            (loop repeat 8
                  collect (bt:make-thread (lambda () (loop repeat 100000 do (prom:counter.inc m))))))
      (is (prom:metric-value m) 800000)))

  (subtest "Integers stay exact, floats switch to double"
    (let ((c (prom:make-counter :name "qwe" :help "" :registry nil)))
      (prom:counter.inc c :value (expt 2 62))
      (is (prom:metric-value c) (expt 2 62))
      (prom:counter.inc c :value 0.5d0)
      (is (prom:metric-value c) (+ (coerce (expt 2 62) 'double-float) 0.5d0))
      (prom:counter.reset c)
      (is (prom:metric-value c) 0)))

  #+sbcl
  (subtest "Nothing allocated on inc"
    (flet ((consed (thunk)
             (funcall thunk)
             (loop repeat 5
                   minimize (let ((before (sb-ext:get-bytes-consed)))
                              (funcall thunk)
                              (- (sb-ext:get-bytes-consed) before)))))
      (let* ((m (prom:get-metric (prom:make-counter :name "qwe" :help "" :registry nil) nil))
             (im (prom:get-metric (prom:make-int-counter :name "qwe" :help "" :registry nil) nil))
             (d 0.25d0))
        (is (consed (lambda () (dotimes (i 100000) (prom:counter.inc m)))) 0 "counter.inc fixnum")
        (is (consed (lambda () (dotimes (i 100000) (prom:counter.inc m :value d)))) 0 "counter.inc double")
        (is (consed (lambda () (dotimes (i 100000) (prom:counter.inc im :value 3)))) 0 "int-counter inc")))))

(subtest "Series lookup"
  (subtest "Concurrent create and lookup"
    (let ((c (prom:make-counter :name "qwe" :help "" :labels '("path") :registry nil))
          (paths (loop for i below 50 collect (format nil "/p~a" i))))
      (mapc #'bt:join-thread
            (loop repeat 8
                  collect (bt:make-thread
                           (lambda ()
                             (loop repeat 200
                                   do (dolist (p paths)
                                        (prom:counter.inc c :labels (list p))))))))
      (is (length (prom::get-metrics c)) 50)
      (ok (every (lambda (p) (= (prom:metric-value (prom:get-metric c (list p))) 1600)) paths))))

  (subtest "Validated on creation"
    (let ((c (prom:make-counter :name "qwe" :help "" :labels '("path") :registry nil)))
      (is-error (prom:counter.inc c :labels '(1)) prom:invalid-label-value-error)
      (is-error (prom:counter.inc c :labels '("a" "b")) prom:invalid-label-count-error)
      (is (length (prom::get-metrics c)) 0)))

  (subtest "Stored labels are a copy"
    (let* ((c (prom:make-counter :name "qwe" :help "" :labels '("path") :registry nil))
           (labels (list (copy-seq "/a"))))
      (prom:counter.inc c :labels labels)
      (setf (first labels) "/b")
      (is (prom:metric-labels (first (prom::get-metrics c))) '("/a"))))

  #+sbcl
  (subtest "Nothing allocated on labelled inc"
    (let* ((c (prom:make-counter :name "qwe" :help "" :labels '("path") :registry nil))
           (labels (list "/a")))
      (prom:counter.inc c :labels labels)
      (is (loop repeat 5
                minimize (let ((before (sb-ext:get-bytes-consed)))
                           (dotimes (i 100000) (prom:counter.inc c :labels labels))
                           (- (sb-ext:get-bytes-consed) before)))
          0))))

(finalize)
