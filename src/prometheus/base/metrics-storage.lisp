(in-package #:prometheus)

;;; Copy-on-write, read-only for readers: an immutable vector of buckets,
;;; indexed by SXHASH of the label values. A lookup writes nothing, so readers
;;; on different CPUs share the vector without contention (GETHASH updates a
;;; cache slot in the table). A writer rebuilds the vector under the lock.

(defun %make-series-table (series)
  (let* ((size (max 8 (ash 1 (integer-length (* 2 (length series))))))
         (table (make-array size :initial-element nil)))
    (dolist (s series table)
      (push s (svref table (logand (sxhash (metric-labels s)) (1- size)))))))

(defclass ht-metrics-storage (synchronizable)
  ((table :initform (%make-series-table nil) :accessor metrics-storage-table)
   (series :initform nil :accessor metrics-storage-series)))

(declaim (inline %find-series))
(defun %find-series (table labels)
  (declare (type simple-vector table))
  (loop for s in (svref table (logand (sxhash labels) (1- (length table))))
        when (equal labels (metric-labels s))
          return s))

(defgeneric get-metric (thing labels)
  (:method ((storage ht-metrics-storage) labels)
    (%find-series (metrics-storage-table storage) labels)))

(defgeneric get-metrics (thing)
  (:method ((storage ht-metrics-storage))
    (copy-list (metrics-storage-series storage))))

(defun %publish-metric (storage metric)
  ;; caller holds the storage lock
  (let* ((series (cons metric (metrics-storage-series storage)))
         (table (%make-series-table series)))
    #+sbcl (sb-thread:barrier (:write))
    (setf (metrics-storage-series storage) series
          (metrics-storage-table storage) table)
    metric))

(defgeneric add-metric (storage metric)
  (:method ((storage ht-metrics-storage) metric)
    (synchronize storage
      (%publish-metric storage metric))))

(defgeneric get-or-add-metric (storage labels mfactory)
  (:method ((storage ht-metrics-storage) labels mfactory)
    (or (get-metric storage labels)
        (synchronize storage
          (or (get-metric storage labels)
              (%publish-metric storage (funcall mfactory (copy-list labels))))))))
