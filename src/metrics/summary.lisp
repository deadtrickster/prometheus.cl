(in-package #:prometheus)

(define-constant +summary-default+ 0)

(defclass summary (metric-family)
  ()
  (:default-initargs :type "summary"))

(defmethod mf-make-metric ((metric summary) labels)
  (make-instance 'summary-metric :labels labels))

(defclass summary-metric (metric)
  ((value :initform +summary-default+)
   (sum :initform +summary-default+ :reader summary-sum)))

(defgeneric summary.observe (summary value &key labels)
  (:method ((summary summary) value &key labels)
    (synchronize summary
      (let ((metric (get-metric summary labels)))
        (summary.observe metric value))))
  (:method ((summary summary-metric) value &key labels)
    (declare (ignore labels))
    (synchronize summary
      (incf (slot-value summary 'value))
      (incf (slot-value summary 'sum) value))))

(defun make-summary (&key name help labels value (registry *default-registry*))
  (assert (not (and labels value)) nil "Can only specify at most one of value and labels.")
  (let ((summary (make-instance 'summary :name name
                                         :help help
                                         :labels labels)))
    (when value
      (summary.observe summary value))
    (when registry
      (register summary registry))
    summary))
