(in-package #:prometheus)

(define-constant +gauge-default+ 0)

(defclass gauge (metric-family)
  ()
  (:default-initargs :type "gauge"))

(defmethod mf-make-metric ((metric gauge) labels)
  (make-gauge-metric :labels labels))

(defstruct (gauge-metric (:include simple-metric) (:copier nil) (:predicate nil))
  (value +gauge-default+))

(defmethod metric-value ((gauge gauge-metric))
  (gauge-metric-value gauge))

(declaim (inline check-gauge-value))
(defun check-gauge-value (value)
  (unless (numberp value)
    (error 'invalid-value-error :value value :reason "value is not a number")))

(defun gauge.set (gauge value &key labels)
  "Set GAUGE, a family or a series from GET-METRIC. Passing the series skips
the label lookup."
  (check-gauge-value value)
  ;; a slot store is atomic, no CAS needed
  (setf (gauge-metric-value (if (typep gauge 'gauge-metric) gauge (get-metric gauge labels)))
        value)
  nil)

(defun gauge.reset (gauge &key labels)
  (setf (gauge-metric-value (if (typep gauge 'gauge-metric) gauge (get-metric gauge labels)))
        +gauge-default+)
  nil)

(defun make-gauge (&key name help labels value (registry *default-registry*))
  (check-value-or-labels value labels)
  (let ((gauge (make-instance 'gauge :name name
                                     :help help
                                     :labels labels
                                     :registry registry)))
    (when value
      (gauge.set gauge value))
    gauge))
