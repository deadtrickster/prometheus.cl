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
      (is-error-report (make-instance 'prom::metric-family :name "qwe" :labels '(123)) prom:invalid-label-name-error "Label name 123 is invalid. Reason: label name is not a string"))))

(finalize)
