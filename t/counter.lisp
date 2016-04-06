(in-package :prometheus.test)

(plan 1)

(subtest "Counter"
  (subtest "Errors & Validations"
    (is-error-report (prom:counter.inc nil :value "qwe") prom:invalid-value-error "Value \"qwe\" is invalid. Reason: value is not a number")
    (is-error-report (prom:counter.inc nil :value -1) prom:invalid-value-error "Value -1 is invalid. Reason: counters can only be incremented by non-negative amounts")
    (is-error-report (prom:make-counter :name "qwe" :value -1 :registry nil) prom:invalid-value-error "Value -1 is invalid. Reason: counters can only be incremented by non-negative amounts")
    (is-error-report (prom:make-counter :name "qwe" :value 12 :labels '("qwe") :registry nil) prom:invalid-value-error "Value 12 is invalid. Reason: can only specify at most one of value and labels"))

  (subtest "INC & RESET"
    (let* ((c (prom:make-counter :name "qwe" :value 12 :registry nil))
           (nlm (prom:get-metric c nil)))
      (prom:counter.inc nlm :value 2)
      (prom:counter.inc nlm)
      (is (prom:metric-value nlm) 15)
      (prom:counter.reset c)
      (is (prom:metric-value nlm) prom::+counter-default+)))

  (subtest "REGISTRY"
    (with-fresh-registry
      (let ((c (prom:make-counter :name "qwe" :value 12)))
        (is (prom:registeredp c prom:*default-registry*) c)))))

(finalize)
