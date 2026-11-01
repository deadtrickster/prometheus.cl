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
  ;; any case: the text format lowercases label names
  (when-let ((label (find "quantile" labels :test #'string-equal)))
    (error 'invalid-label-name-error :name label :reason "summary cannot have a label named \"quantile\""))
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
  ;; a simple summary takes COUNT pre-aggregated observations summing to
  ;; VALUE; a sketch cannot place those in buckets
  (unless (eql count 1)
    (error 'invalid-value-error :value count :reason "quantile summary observes one value at a time, count should be 1"))
  (sharded-ddsketch-observe (quantile-summary-sharded-sketch summary) value))

(defmethod summary-count ((summary quantile-summary-metric))
  "A read of its own: for a count consistent with the sum and quantiles use
WITH-QUANTILE-SUMMARY-READ."
  (sharded-ddsketch-count (quantile-summary-sharded-sketch summary)))

(defmethod summary-sum ((summary quantile-summary-metric))
  "A read of its own: for a sum consistent with the count and quantiles use
WITH-QUANTILE-SUMMARY-READ."
  (sharded-ddsketch-sum (quantile-summary-sharded-sketch summary)))

(defmacro with-quantile-summary-read ((quantiles estimates count sum) metric &body body)
  "Run BODY with QUANTILES, ESTIMATES, COUNT and SUM from one read of METRIC,
so they describe the same observations, while holding the metric's lock.
ESTIMATES is the scrape buffer and only meaningful when COUNT is positive.
Allocates nothing; meant for exposition code."
  (let ((m (gensym "METRIC")) (qs (gensym "QS")) (out (gensym "OUT"))
        (total (gensym "TOTAL")) (s (gensym "SUM")))
    `(let ((,m ,metric))
       (synchronize ,m
         (let ((,qs (quantile-summary-metric-quantiles ,m))
               (,out (quantile-summary-metric-estimates ,m)))
           ;; typed, so the body's AREFs are not generic calls
           (declare (type quantile-buffer ,qs ,out))
           (%with-sharded-read (,(gensym "OBSERVED") ,total ,s)
               ((quantile-summary-sharded-sketch ,m) ,qs ,out)
             ;; all four in one LET, so BODY may declare any of them
             (let ((,quantiles ,qs)
                   (,estimates ,out)
                   (,count (%count->integer ,total))
                   (,sum ,s))
               (declare (type quantile-buffer ,quantiles ,estimates))
               ,@body)))))))

(defmacro with-quantile-summary-estimates ((quantiles estimates) metric &body body)
  "Like WITH-QUANTILE-SUMMARY-READ without count and sum. BODY is not run
when nothing has been observed."
  (let ((count (gensym "COUNT")) (sum (gensym "SUM")))
    `(with-quantile-summary-read (,quantiles ,estimates ,count ,sum) ,metric
       (declare (ignore ,sum))
       (when (plusp ,count)
         (locally ,@body)))))

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
sketch.  INTO is checked before it is reset, and left alone when no
series matches, in which case NIL is returned."
  (let ((metrics (remove-if-not (lambda (m) (funcall test (metric-labels m)))
                                (get-metrics summary))))
    (when metrics
      (flet ((shards (m) (sharded-ddsketch-shards (quantile-summary-sharded-sketch m))))
        ;; every check before INTO is reset, so a bad INTO keeps its data
        (when into
          (dolist (m metrics) (check-merge-target into (shards m))))
        (let ((result (if into (ddsketch-reset into) (ddsketch-empty-copy (svref (shards (first metrics)) 0)))))
          (dolist (m metrics result)
            (loop for shard across (shards m) do (ddsketch-merge result shard))))))))

(defun make-quantile-summary (&key name help labels value quantiles
                                (relative-error 0.01d0) (min-value 1d-9) (max-value 1d9) (shards (default-shards))
                                (registry *default-registry*))
  "Make a summary whose QUANTILES (default 0.5, 0.9, 0.95) are estimated with
a DDSketch: within RELATIVE-ERROR of the true value for observations in
[MIN-VALUE, MAX-VALUE].  Observations must be non-negative.

Each series has SHARDS sketches (default one per CPU), combined at scrape
time and all allocated with the series: about 34KB each with the default
range and error."
  (check-value-or-labels value labels)
  (check-ddsketch-parameters relative-error min-value max-value shards)
  ;; before registering, so a bad value does not leave the name taken
  (when (and value (not (and (realp value) (<= 0 value most-positive-double-float))))
    (%bad-sketch-value value))
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
