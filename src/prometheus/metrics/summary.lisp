(in-package #:prometheus)

(define-constant +summary-default+ 0)

(defclass summary-base (metric-family)
  ()
  (:default-initargs :type "summary"))

(defclass simple-summary (summary-base)
  ())

(defmethod mf-make-metric ((metric simple-summary) labels)
  (make-instance 'simple-summary-metric :labels labels))

(defclass simple-summary-metric (metric)
  ((value :initform +summary-default+ :reader summary-count)
   (sum :initform +summary-default+ :reader summary-sum)))

(defclass summary (summary-base)
  ((quantiles :initarg :quantiles :reader summary-quantiles%)))

(defun create-summary-estimator (summary)
  (apply #'quantile-estimator:make-estimator (summary-quantiles% summary)))

(defmethod mf-make-metric ((metric summary) labels)
  (make-instance 'summary-metric :labels labels
                                 :estimator (create-summary-estimator metric)))

(defclass summary-metric (metric)
  ((estimator :initarg :estimator :reader summary-estimator)
   (mutex :initform (bt:make-lock "prometheus summary lock") :reader summary-mutex)
   (count :initform +summary-default+ :reader summary-count)
   (sum :initform +summary-default+ :reader summary-sum)))

(defun summary-quantiles (summary)
  (let ((estimator (summary-estimator summary)))
    (loop for quantile in (quantile-estimator:estimator-invariants estimator)
          collect (cons (quantile-quantile quantile)
                        (estimator.query estimator (quantile-quantile quantile))))))

(defmethod summary-count ((summary summary-metric))
  (estimator-observations (summary-estimator summary)))

(defmethod summary-sum ((summary summary-metric))
  (estimator-sum (summary-estimator summary)))

(defun check-summary-value (value)
  (unless (numberp value)
    (error 'invalid-value-error :value value :reason "value is not a number")))

(defgeneric summary.observe% (summary value count labels)
  (:method ((summary summary-base) value count labels)
    (let ((metric (get-metric summary labels)))
      (summary.observe% metric value count nil)))
  (:method ((summary simple-summary-metric) value count labels)
    (declare (ignore labels))
    #-(or sbcl lispworks)
    (synchronize summary
      (incf (slot-value summary 'value) count)
      (incf (slot-value summary 'sum) value))
    #+(or sbcl lispworks)
    (progn
      (cas-incf (slot-value summary 'value) count)
      (cas-incf (slot-value summary 'sum) value)))
  (:method ((summary summary-metric) value count labels)
    (declare (ignore labels))
    (assert (= count 1))
    (bt:with-lock-held ((summary-mutex summary))
      (estimator.observe (summary-estimator summary)
                         value)
      (estimator-sum (summary-estimator summary)))))

(defun summary.observe (summary value &key (count 1) labels)
  (check-summary-value value)
  (summary.observe% summary value count labels))

(defmacro summary.time (summary &body body)
  `(timing% (lambda (time)
              (summary.observe ,summary time))
            (lambda () ,@body)))

(defun make-simple-summary (&key name help labels value (count 1) (registry *default-registry*))
  (check-value-or-labels value labels)
  (let ((summary (make-instance 'simple-summary :name name
                                                :help help
                                                :labels labels
                                                :registry registry)))
    (when value
      (summary.observe summary value :count count))
    summary))

(defun quantiles-from-alist (alist)
  (loop for (quantile . error) in alist
        collect (make-quantile quantile error)))

(defun default-quantiles ()
  (quantiles-from-alist
   '((0.5d0 . 0.05d0)
     (0.9d0 . 0.01d0)
     (0.99d0 . 0.001d0))))

(defun normailize-quantiles (quantiles)
  (if quantiles
      (loop for quantile in quantiles
            collect (if (typep quantile 'quantile-estimator::quantile)
                        quantile
                        (make-quantile (car quantile) (cdr quantile))))
      (default-quantiles)))

(defun make-summary (&key name help labels value quantiles (registry *default-registry*))
  (check-value-or-labels value labels)
  (let ((summary (make-instance 'summary :name name
                                         :help help
                                         :labels labels
                                         :registry registry
                                         :quantiles (normailize-quantiles quantiles))))
    (when value
      (summary.observe summary value))
    summary))
