(in-package #:prometheus)

(define-constant +summary-default+ 0)

(defclass summary-base (metric-family)
  ()
  (:default-initargs :type "summary"))

(defclass simple-summary (summary-base)
  ())

(defmethod mf-make-metric ((metric simple-summary) labels)
  (make-simple-summary-metric :labels labels))

(defgeneric summary-count (summary))
(defgeneric summary-sum (summary))

(defstruct (simple-summary-metric (:include metric) (:copier nil) (:predicate nil))
  (count +summary-default+)
  (sum +summary-default+))

(defmethod metric-value ((summary simple-summary-metric))
  (simple-summary-metric-count summary))

(defmethod summary-count ((summary simple-summary-metric))
  (simple-summary-metric-count summary))

(defmethod summary-sum ((summary simple-summary-metric))
  (simple-summary-metric-sum summary))

(defclass summary (summary-base)
  ((quantiles :initarg :quantiles :reader summary-quantiles%)))

(defun create-summary-estimator (summary)
  (apply #'quantile-estimator:make-estimator (summary-quantiles% summary)))

(defmethod mf-make-metric ((metric summary) labels)
  (make-summary-metric :labels labels
                       :estimator (create-summary-estimator metric)))

(defstruct (summary-metric (:include metric) (:copier nil) (:predicate nil))
  (estimator nil :read-only t))

(declaim (inline summary-estimator))
(defun summary-estimator (summary)
  (summary-metric-estimator summary))

(defgeneric summary-quantiles (summary))

(defmethod summary-quantiles ((summary summary-metric))
  (let ((estimator (summary-estimator summary)))
    (loop for quantile in (quantile-estimator:estimator-invariants estimator)
          collect (cons (quantile-quantile quantile)
                        (estimator.query estimator (quantile-quantile quantile))))))

(defmethod summary-count ((summary summary-metric))
  (estimator-observations (summary-estimator summary)))

(defmethod summary-sum ((summary summary-metric))
  (estimator-sum (summary-estimator summary)))

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
