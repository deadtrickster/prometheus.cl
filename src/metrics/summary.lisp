(in-package #:prometheus)

(define-constant +summary-default+ 0)

(defclass summary (metric-family)
  ()
  (:default-initargs :type "summary"))

(defmethod mf-make-metric ((metric summary) labels)
  (make-instance 'summary-metric :labels labels))

(defclass summary-metric (metric)
  ((value :initform +summary-default+ :reader summary-count)
   (sum :initform +summary-default+ :reader summary-sum)))

(defun check-summary-value (value)
  (unless (numberp value)
    (error 'invalid-value-error :value value :reason "value is not a number")))

(defgeneric summary.observe% (summary value count labels)
  (:method ((summary summary) value count labels)
    (synchronize summary
      (let ((metric (get-metric summary labels)))
        (summary.observe% metric value count nil))))
  (:method ((summary summary-metric) value count labels)
    (declare (ignore labels))
    (synchronize summary
      (incf (slot-value summary 'value) count)
      (incf (slot-value summary 'sum) value))))

(defun summary.observe (summary value &key (count 1) labels)
  (check-summary-value value)
  (summary.observe% summary value count labels))

(defmacro summary.time (summary &body body)
  `(timing% (lambda (time)
              (summary.observe ,summary time))
            (lambda () ,@body)))

(defun make-summary (&key name help labels value (count 1) (registry *default-registry*))
  (check-value-or-labels value labels)
  (let ((summary (make-instance 'summary :name name
                                         :help help
                                         :labels labels)))
    (when value
      (summary.observe summary value :count count))
    (when registry
      (register summary registry))
    summary))
