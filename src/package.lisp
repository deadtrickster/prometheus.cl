(in-package #:cl-user)

(defpackage #:prometheus
  (:use #:cl #:alexandria)
  (:nicknames #:prom)
  (:export *default-registry*
           #:registry
           #:register
           #:collect
           #:collector
           #:collector-metric-name
           #:metric-family-name
           #:metric-family-type
           #:metric-family-help
           #:metric-family-labels
           #:get-metric
           #:metric
           #:metric-value
           #:metric-labels
           #:simple-metric
           ;; strandard metric types
           #:counter
           #:make-counter
           #:counter-metric
           #:counter.inc
           #:counter.reset
           #:gauge
           #:make-gauge
           #:gauge-metric
           #:gauge.set
           #:gauge.reset
           #:bucket-bound
           #:bucket-count
           #:histogram
           #:histogram-metric
           #:histogram-sum
           #:make-histogram
           #:histogram.observe
           #:histogram.time
           #:summary
           #:summary-metric
           #:summary-sum
           #:make-summary
           #:summary.observe
           #:summary.time
           ;; errors
           #:base-error
           #:invalid-label-name-error
           #:invalid-label-value-error
           #:invalid-label-count-error
           #:invalid-metric-name-error))
