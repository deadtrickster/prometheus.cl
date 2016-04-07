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

(define-method-combination validator ()
         ((primary () :order :most-specific-last :required t))
   (let ((form (if (rest primary)
                   `(progn ,@(mapcar #'(lambda (method)
                                         `(call-method ,method))
                                     primary))
                   `(call-method ,(first primary)))))
     form))

(defgeneric validate-args (mv &rest initargs &key &allow-other-keys)
  (:method-combination validator)
  (:method ((mf metric-family) &rest initargs &key name labels &allow-other-keys)
    (declare (ignore initargs))
    (check-metric-name name)
    (check-label-names labels)))

(defmethod initialize-instance :before ((mf metric-family) &rest initargs)
  (apply #'validate-args mf initargs))

(defmethod initialize-instance :after ((mf metric-family) &rest initargs &key registry &allow-other-keys)
  (declare (ignore initargs))
  (when registry
    (register mf registry)))

(defgeneric mf-make-metric (metric-family labels))

(defmethod collect ((mf metric-family) cb)
  (funcall cb mf))

(defmethod get-metric ((mf metric-family) labels)
  (check-label-values labels (metric-family-labels mf))
  (or (get-metric (metric-family-metrics mf) labels)
      (add-metric (metric-family-metrics mf) (mf-make-metric mf labels))))

(defmethod get-metrics ((mf metric-family))
  (get-metrics (metric-family-metrics mf)))

(defclass metric (synchronizable)
  ((labels :initform nil :initarg :labels :reader metric-labels)
   (value :initarg :value :reader metric-value)))

(defclass simple-metric (metric)
  ())

(defun check-value-or-labels (value labels)
  (assert (not (and labels value)) nil 'invalid-value-error :value value :reason "can only specify at most one of value and labels"))
