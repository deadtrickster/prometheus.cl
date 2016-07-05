(in-package :prometheus.test)

(plan 1)

(subtest "Gauge"
  (subtest "Errors & Validations"
    (is-error-report (prom:gauge.set nil "qwe") prom:invalid-value-error "Value \"qwe\" is invalid. Reason: value is not a number")
    (is-error-report (prom:make-gauge :name "qwe" :help "" :value 12 :labels '("qwe") :registry nil) prom:invalid-value-error "Value 12 is invalid. Reason: can only specify at most one of value and labels"))

  (subtest "INC & RESET"
    (let* ((g (prom:make-gauge :name "qwe" :help "" :value 12 :registry nil))
           (nlm (prom:get-metric g nil)))
      (is (prom:metric-value nlm) 12)
      (prom:gauge.set nlm 2)
      (is (prom:metric-value nlm) 2)
      (prom:gauge.reset g)
      (is (prom:metric-value nlm) prom::+gauge-default+)))

  (subtest "REGISTRY"
    (with-fresh-registry
      (let ((g (prom:make-gauge :name "qwe" :help "" :value 12)))
        (is (prom:registeredp g prom:*default-registry*) g)))))

(finalize)
