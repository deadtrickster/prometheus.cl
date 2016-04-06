(in-package #:prometheus.test)

(plan 1)

(subtest "Labels"
  (subtest "Label name"
    (subtest "Check label name"
      (is-error-report (prom::check-label-name 123) prom:invalid-label-name-error "Label name 123 is invalid. Reason: label name is not a string")
      (is-error-report (prom::check-label-name "job") prom:invalid-label-name-error "Label name \"job\" is invalid. Reason: label name is reserved")
      (is-error-report (prom::check-label-name "__qwe") prom:invalid-label-name-error "Label name \"__qwe\" is invalid. Reason: label name starts with __")
      (is-error-report (prom::check-label-name "123qwe") prom:invalid-label-name-error "Label name \"123qwe\" is invalid. Reason: label name doesn't match regex [a-zA-Z_][a-zA-Z0-9_]*")
      (is-error-report (prom::check-label-name "") prom:invalid-label-name-error "Label name \"\" is invalid. Reason: label name doesn't match regex [a-zA-Z_][a-zA-Z0-9_]*")
      (is-error-report (prom::check-label-name "qwe:qwe") prom:invalid-label-name-error "Label name \"qwe:qwe\" is invalid. Reason: label name doesn't match regex [a-zA-Z_][a-zA-Z0-9_]*"))
    (subtest "Validated on metric creation"
      (is-error-report (make-instance 'prom::metric-family :name "qwe" :labels '(123)) prom:invalid-label-name-error "Label name 123 is invalid. Reason: label name is not a string")))

  (subtest "Label values"
    (subtest "Check label values"
      (is-error-report (prom::check-label-values '(123) '("qwe")) prom:invalid-label-value-error "Label value 123 is invalid. Reason: label value is not a string"))
    (subtest "Validates on get-metric"
      (let ((mf (make-instance 'prom::metric-family :name "qwe" :labels '("qwe"))))
        (is-error-report (prom:get-metric mf '()) prom:invalid-label-count-error "Invalid label count. Got 0, expected 1")
        (is-error-report (prom:get-metric mf '(123)) prom:invalid-label-value-error "Label value 123 is invalid. Reason: label value is not a string")))))

(finalize)
