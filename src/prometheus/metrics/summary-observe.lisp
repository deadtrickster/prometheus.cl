(in-package #:prometheus)

(defun summary.observe (summary value &key (count 1) labels)
  "Observe VALUE COUNT times on SUMMARY, a family or a series from GET-METRIC.
Passing the series skips the label lookup."
  (unless (numberp value)
    (error 'invalid-value-error :value value :reason "value is not a number"))
  (let ((series (if (typep summary 'metric) summary (get-metric summary labels))))
    (etypecase series
      (quantile-summary-metric
       ;; a simple summary takes COUNT pre-aggregated observations summing
       ;; to VALUE; a sketch cannot place those in buckets
       (unless (eql count 1)
         (error 'invalid-value-error :value count :reason "quantile summary observes one value at a time, count should be 1"))
       (sharded-ddsketch-observe (quantile-summary-metric-sketch series) value))
      (simple-summary-metric
       #+(or sbcl lispworks)
       (progn
         (cas-incf (simple-summary-metric-count series) count)
         (cas-incf (simple-summary-metric-sum series) value))
       #-(or sbcl lispworks)
       (synchronize series
         (incf (simple-summary-metric-count series) count)
         (incf (simple-summary-metric-sum series) value)))
      (summary-metric
       (assert (= count 1))
       (synchronize series
         (estimator.observe (summary-metric-estimator series) value)))))
  nil)

(defmacro summary.time (summary &body body)
  `(timing% (lambda (time)
              (summary.observe ,summary time))
            (lambda () ,@body)))
