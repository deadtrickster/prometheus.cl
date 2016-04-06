(in-package #:prometheus)

(define-constant +gauge-default+ 0)

(defclass gauge (metric-family)
  ()
  (:default-initargs :type "gauge"))

(defmethod mf-make-metric ((metric gauge) labels)
  (make-instance 'gauge-metric :labels labels))

(defclass gauge-metric (simple-metric)
  ((value :initform +gauge-default+)))

(defun check-gauge-value (value)
  (unless (numberp value)
    (error 'invalid-value-error :value value :reason "value is not a number")))

(defgeneric gauge.set% (gauge value labels)
  (:method ((gauge gauge) value labels)
    (synchronize gauge
      (let ((metric (get-metric gauge labels)))
        (gauge.set% metric value labels))))
  (:method ((gauge gauge-metric) value labels)
    (declare (ignore labels))
    (synchronize gauge
      (setf (slot-value gauge 'value) value))))

(defun gauge.set (gauge value &key labels)
  (check-gauge-value value)
  (gauge.set% gauge value labels))

(defgeneric gauge.reset (gauge &key labels)
  (:method ((gauge gauge) &key labels)
    (synchronize gauge
      (let ((metric (get-metric gauge labels)))
        (gauge.reset metric))))
  (:method ((gauge gauge-metric) &key labels)
    (declare (ignore labels))
    (synchronize gauge
      (setf (slot-value gauge 'value) +gauge-default+))))

(defun make-gauge (&key name help labels value (registry *default-registry*))
  (check-value-or-labels value labels)
  (let ((gauge (make-instance 'gauge :name name
                                     :help help
                                     :labels labels)))
    (when value
      (gauge.set gauge value))
    (when registry
      (register gauge registry))
    gauge))
