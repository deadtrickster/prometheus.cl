(in-package #:prometheus)

(defun check-metric-name-is-string (name)
  (unless (stringp name)
    (error 'invalid-metric-name-error :name name :reason "metric name is not a string")))

(defun check-metric-name-regex (name)
  (unless (equal name (ppcre:scan-to-strings "[a-zA-Z_:][a-zA-Z0-9_:]*" name))
    (error 'invalid-metric-name-error :name name :reason "metric name doesn't match regex [a-zA-Z_:][a-zA-Z0-9_:]*")))

(defun check-metric-name (name)
  (check-metric-name-is-string name)
  (check-metric-name-regex name))

(defclass metric-family (collectable synchronizable)
  ((name :initarg :name :reader metric-family-name)
   (help :initform nil :initarg :help :reader metric-family-help)
   (type :initform "untyped" :initarg :type :reader metric-family-type)
   (labels :initform nil :initarg :labels :reader metric-family-labels)
   (metrics :initform (make-instance 'ht-metrics-storage) :initarg :metrics :reader metric-family-metrics)))

(defmethod initialize-instance :before ((mf metric-family) &rest initargs &key name labels &allow-other-keys)
  (declare (ignore initargs))
  (check-metric-name name)
  (check-labels-names labels)
  (when (next-method-p)
    (call-next-method)))

(defgeneric mf-make-metric (metric-family labels))

(defmethod collect ((mf metric-family) cb)
  (funcall cb mf))

(defmethod get-metric ((mf metric-family) labels)
  (assert (listp labels) nil "Labels values must be list. Go ~a ~a" (type-of labels) labels)
  (assert (= (length (metric-family-labels mf)) (length labels))
          nil "Invalid label count ~a" (length labels))
  (assert (validate-label-values labels)
          nil "Label values must be string")
  (or (get-metric (metric-family-metrics mf) labels)
      (add-metric (metric-family-metrics mf) (mf-make-metric mf labels))))

(defmethod get-metrics ((mf metric-family))
  (get-metrics (metric-family-metrics mf)))

(defclass metric (synchronizable)
  ((labels :initform nil :initarg :labels :reader metric-labels)
   (value :initarg :value :reader metric-value)))

(defclass simple-metric (metric)
  ())
