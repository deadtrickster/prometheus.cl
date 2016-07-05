(in-package :prometheus.test)

(plan 2)

(subtest "Simple Summary"
  (subtest "Errors & Validations"
    (is-error-report (prom:summary.observe nil "qwe") prom:invalid-value-error "Value \"qwe\" is invalid. Reason: value is not a number")
    (is-error-report (prom:make-summary :name "qwe" :help "" :value 12 :labels '("qwe") :registry nil) prom:invalid-value-error "Value 12 is invalid. Reason: can only specify at most one of value and labels"))

  (subtest "OBSERVE"
    (let* ((s (prom:make-simple-summary :name "qwe" :help "" :value 12 :count 2 :registry nil))
           (nlm (prom:get-metric s nil)))
      (is (prom:summary-sum nlm) 12)
      (is (prom:summary-count nlm) 2)
      (prom:summary.observe nlm 2)
      (is (prom:summary-sum nlm) 14)
      (is (prom:summary-count nlm) 3)))

  (subtest "TIME"
    (let* ((s (prom:make-simple-summary :name "qwe" :help "" :registry nil))
           (nlm (prom:get-metric s nil)))
      (prom:summary.time nlm (sleep 0.5))
      (ok (and (>= (prom:summary-sum nlm) 500)
               (< (prom:summary-sum nlm) 510)))
      (is (prom:summary-count nlm) 1)))

  (subtest "REGISTRY"
    (with-fresh-registry
      (let ((s (prom:make-simple-summary :name "qwe" :help "" :value 12)))
        (is (prom:registeredp s prom:*default-registry*) s)))))

(subtest "Summary"
  (subtest "Errors & Validations"
    (is-error-report (prom:summary.observe nil "qwe") prom:invalid-value-error "Value \"qwe\" is invalid. Reason: value is not a number")
    (is-error-report (prom:make-summary :name "qwe" :help "" :value 12 :labels '("qwe") :registry nil) prom:invalid-value-error "Value 12 is invalid. Reason: can only specify at most one of value and labels"))

  (subtest "OBSERVE"
    (let* ((s (prom:make-summary :name "qwe" :help "" :value 3 :registry nil))
           (nlm (prom:get-metric s nil)))
      (is (prom:summary-sum nlm) 3)
      (is (prom:summary-count nlm) 1)
      (prom:summary.observe nlm 5.2)
      (prom:summary.observe nlm 13)
      (prom:summary.observe nlm 4)
      (is (prom:summary-sum nlm) 25.2)
      (is (prom:summary-count nlm) 4)
      (is (prom:summary-quantiles nlm) '((0.5d0 . 4) (0.9d0 . 5.2) (0.99d0 . 5.2)))))

  (subtest "TIME"
    (let* ((s (prom:make-summary :name "qwe" :help "" :registry nil))
           (nlm (prom:get-metric s nil)))
      (prom:summary.time nlm (sleep 0.5))
      (ok (and (>= (prom:summary-sum nlm) 500)
               (< (prom:summary-sum nlm) 510)))
      (is (prom:summary-count nlm) 1)))

  (subtest "REGISTRY"
    (with-fresh-registry
      (let ((s (prom:make-simple-summary :name "qwe" :help "" :value 12)))
        (is (prom:registeredp s prom:*default-registry*) s)))))

(finalize)
