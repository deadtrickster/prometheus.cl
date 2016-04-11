(in-package :prometheus.test)

(plan 1)

(subtest "Counter"
  (subtest "Errors & Validations"
    (is-error-report (prom:counter.inc nil :value "qwe") prom:invalid-value-error "Value \"qwe\" is invalid. Reason: value is not a number")
    (is-error-report (prom:counter.inc nil :value -1) prom:invalid-value-error "Value -1 is invalid. Reason: counters can only be incremented by non-negative amounts")
    (is-error-report (prom:make-counter :name "qwe" :value -1 :registry nil) prom:invalid-value-error "Value -1 is invalid. Reason: counters can only be incremented by non-negative amounts")
    (is-error-report (prom:make-counter :name "qwe" :value 12 :labels '("qwe") :registry nil) prom:invalid-value-error "Value 12 is invalid. Reason: can only specify at most one of value and labels"))

  (subtest "INC & RESET"
    (subtest "No labels"
      (let* ((c (prom:make-counter :name "qwe" :value 12 :registry nil)))
        (prom:counter.inc c :value 2)
        (prom:counter.inc c)
        (is (prom:metric-value c) 15)
        (prom:counter.reset c)
        (is (prom:metric-value c) prom::+counter-default+)))
    (subtest "With labels"
      (let* ((c (prom:make-counter :name "qwe" :labels '("method") :registry nil)))
        (prom:counter.inc c :value 2 :labels '("get"))
        (prom:counter.inc c :labels '("get"))
        (is (prom:metric-value (prom:get-metric c '("get"))) 3)
        (prom:counter.reset c :labels '("get"))
        (is (prom:metric-value (prom:get-metric c '("get"))) prom::+counter-default+))))

  (subtest "REGISTRY"
    (with-fresh-registry
      (let ((c (prom:make-counter :name "qwe" :value 12)))
        (is (prom:registeredp c prom:*default-registry*) c)))))

(finalize)
