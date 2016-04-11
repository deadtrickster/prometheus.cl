(in-package #:prometheus.test)

(plan 1)

(subtest "Metrics"
  (subtest "Metric name"
    (subtest "Check metric name"
      (is-error-report (prom::check-metric-name 123) prom:invalid-metric-name-error "Metric name 123 is invalid. Reason: metric name is not a string")
      (is-error-report (prom::check-metric-name "123qwe") prom:invalid-metric-name-error "Metric name \"123qwe\" is invalid. Reason: metric name doesn't match regex [a-zA-Z_:][a-zA-Z0-9_:]*")
      (is-error-report (prom::check-metric-name "") prom:invalid-metric-name-error "Metric name \"\" is invalid. Reason: metric name doesn't match regex [a-zA-Z_:][a-zA-Z0-9_:]*"))
    (subtest "Validated on metric creating"
      (is-error-report (make-instance 'prom::metric-family :name 123) prom:invalid-metric-name-error "Metric name 123 is invalid. Reason: metric name is not a string")))
  (subtest "Labels"
    (subtest "No labels"
      (let* ((c (prom:make-counter :name "qwe" :value 12 :registry nil)))
        (is-type (prom::metric-family-no-labels-metric c) 'prom:counter-metric "No-labels metric is counter-metric")
        (is (prom::metric-family-metrics c) nil "Metrics storage is nil")))
    (subtest "With labels"
      (let* ((c (prom:make-counter :name "qwe" :labels '("method") :registry nil)))
        (is (prom::metric-family-no-labels-metric c) nil "No-labels metric is NIL")
        (is-type (prom::metric-family-metrics c) 'prom::ht-metrics-storage "Metrics storage is ht-metrics-storage")))))

(finalize)
