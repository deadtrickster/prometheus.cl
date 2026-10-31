(in-package #:prometheus)

;;; Port of prometheus.erl's prometheus_quantile_summary. See
;;; docs/quantile-summary.md.

(defun default-sketch-quantiles ()
  (list 0.5d0 0.9d0 0.95d0))

(defun normalize-sketch-quantile (q)
  (unless (and (realp q) (<= 0 q 1))
    (error 'invalid-value-error :value q :reason "quantile should be in [0, 1]"))
  ;; 0.95 -> 0.95d0, not 0.949999988079071d0. It is printed as a label.
  (coerce (rationalize q) 'double-float))

(defun normalize-sketch-quantiles (quantiles)
  "Ascending, duplicate-free (SIMPLE-ARRAY DOUBLE-FLOAT (*))."
  (let ((sorted (sort (remove-duplicates (mapcar #'normalize-sketch-quantile
                                                 (or quantiles (default-sketch-quantiles))))
                      #'<)))
    (make-array (length sorted) :element-type 'double-float :initial-contents sorted)))

(defclass quantile-summary (summary-base)
  ((quantiles :initarg :quantiles :reader quantile-summary-quantiles)
   (relative-error :initarg :relative-error :reader quantile-summary-relative-error)
   (min-value :initarg :min-value :reader quantile-summary-min-value)
   (max-value :initarg :max-value :reader quantile-summary-max-value)
   (shards :initarg :shards :reader quantile-summary-shards)))

(defmethod validate-args ((mf quantile-summary) &rest initargs &key labels quantiles &allow-other-keys)
  (declare (ignore initargs))
  (when (find "quantile" labels :test #'equal)
    (error 'invalid-label-name-error :name "quantile" :reason "summary cannot have a label named \"quantile\""))
  (list :quantiles (normalize-sketch-quantiles quantiles)))

(defclass quantile-summary-metric (metric)
  ;; ESTIMATES is the scrape buffer, reused under the metric lock.
  ((quantiles :initarg :quantiles :reader quantile-summary-metric-quantiles)
   (estimates :initarg :estimates :reader quantile-summary-metric-estimates)
   (sketch :initarg :sketch :reader quantile-summary-sharded-sketch)))

(defmethod mf-make-metric ((mf quantile-summary) labels)
  (let ((quantiles (quantile-summary-quantiles mf)))
    (make-instance 'quantile-summary-metric
                   :labels labels
                   :quantiles quantiles
                   :estimates (make-array (length quantiles) :element-type 'double-float
                                                             :initial-element 0d0)
                   :sketch (make-sharded-ddsketch :shards (quantile-summary-shards mf)
                                                  :relative-error (quantile-summary-relative-error mf)
                                                  :min-value (quantile-summary-min-value mf)
                                                  :max-value (quantile-summary-max-value mf)))))

(defmethod summary.observe% ((summary quantile-summary-metric) value count labels)
  (declare (ignore labels))
  (let ((sketch (quantile-summary-sharded-sketch summary)))
    (dotimes (i count)
      (sharded-ddsketch-observe sketch value))))

(defmethod summary-count ((summary quantile-summary-metric))
  (sharded-ddsketch-count (quantile-summary-sharded-sketch summary)))

(defmethod summary-sum ((summary quantile-summary-metric))
  (sharded-ddsketch-sum (quantile-summary-sharded-sketch summary)))

(defmacro with-quantile-summary-estimates ((quantiles estimates) metric &body body)
  "Run BODY with QUANTILES and ESTIMATES bound to METRIC's quantile vector
and its scrape buffer, freshly filled, while holding the metric's lock.
BODY is not run when nothing has been observed.  Allocates nothing; meant
for exposition code."
  (let ((m (gensym "METRIC")))
    `(let ((,m ,metric))
       (synchronize ,m
         (let ((,quantiles (quantile-summary-metric-quantiles ,m))
               (,estimates (quantile-summary-metric-estimates ,m)))
           (when (%sketches-quantiles-into (sharded-ddsketch-shards (quantile-summary-sharded-sketch ,m))
                                           ,quantiles ,estimates)
             ,@body))))))

(defmethod summary-quantiles ((summary quantile-summary-metric))
  "Alist of (quantile . estimate); empty until something is observed.
Conses the alist; exposition code should use WITH-QUANTILE-SUMMARY-ESTIMATES."
  (with-quantile-summary-estimates (qs estimates) summary
    (loop for i from 0 below (length qs)
          collect (cons (aref qs i) (aref estimates i)))))

(defun quantile-summary-merged-sketch (summary &key (test (constantly t)) into)
  "Merge every series of SUMMARY whose label values satisfy TEST, e.g. the
latency distribution across all endpoints.  Merges into INTO, reset first,
when given (a sketch made with the family's parameters), else into a new
sketch.  Returns NIL when no series matches."
  (let ((result nil))
    (dolist (metric (get-metrics summary) result)
      (when (funcall test (metric-labels metric))
        (loop for shard across (sharded-ddsketch-shards (quantile-summary-sharded-sketch metric))
              do (unless result
                   (setf result (if into (ddsketch-reset into) (ddsketch-empty-copy shard))))
                 (ddsketch-merge result shard))))))

(defun make-quantile-summary (&key name help labels value quantiles
                                (relative-error 0.01d0) (min-value 1d-9) (max-value 1d9) (shards 16)
                                (registry *default-registry*))
  "Make a summary whose QUANTILES (default 0.5, 0.9, 0.95) are estimated with
a DDSketch: within RELATIVE-ERROR of the true value for observations in
[MIN-VALUE, MAX-VALUE].  Observations must be non-negative.

Each series is split in SHARDS (a power of two) sketches, one per
thread hash, read together at scrape time.  All shards are allocated with
the series: about 17KB per shard with the default range and error, so
~270KB per series at 16 shards; lower SHARDS or narrow the range for
high-cardinality families."
  (check-value-or-labels value labels)
  (check-ddsketch-parameters relative-error min-value max-value shards)
  (let ((summary (make-instance 'quantile-summary :name name
                                                  :help help
                                                  :labels labels
                                                  :registry registry
                                                  :quantiles quantiles
                                                  :relative-error relative-error
                                                  :min-value min-value
                                                  :max-value max-value
                                                  :shards shards)))
    (when value
      (summary.observe summary value))
    summary))
