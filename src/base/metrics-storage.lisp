(in-package #:prometheus)

(defun labels-sxhash (labels)
  (sxhash labels))

(defun labels-equalp (l1 l2)
  (every #'equal l1 l2))

#+ (or sbcl cmu)
(sb-ext:define-hash-table-test labels-equalp labels-sxhash)

(defun make-metrics-storage-ht ()
  #+(or sbcl allegro ccl lispworks)
  (make-hash-table :test #'labels-equalp :hash-function #'labels-sxhash)
  #-(or sbcl allegro ccl lispworks)
  (error "Your implementation not supported"))

(defclass ht-metrics-storage (synchronizable)
  ((ht :initform (make-metrics-storage-ht) :reader metrics-storage-ht)))

(defgeneric get-metric (thing labels)
  (:method ((storage ht-metrics-storage) labels)
    (gethash labels (metrics-storage-ht storage))))

(defgeneric get-metrics (thing)
  (:method ((storage ht-metrics-storage))
    (hash-table-values (metrics-storage-ht storage))))

(defgeneric add-metric (storage metric)
  (:method ((storage ht-metrics-storage) metric)
    (setf (gethash (metric-labels metric) (metrics-storage-ht storage)) metric)))
