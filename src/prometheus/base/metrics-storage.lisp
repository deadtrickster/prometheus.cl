(in-package #:prometheus)

;;; Series by label values: an open-addressed table, at most half full, read
;;; without a lock. A writer, under the lock, stores a finished series into an
;;; empty slot - one store, readers see the slot empty or filled. Growth
;;; rebuilds into a table twice as big, the only garbage, amortized O(1) per
;;; series, from hashes kept at insert so a rebuild hashes no strings.
;;; Iteration reads an append-only vector up to a published count.

(defun %labels-hash (labels)
  ;; every value counts: SXHASH of a list stops after 4 elements
  (let ((h 0))
    (declare (type (unsigned-byte 62) h))
    (dolist (v labels h)
      (setf h (logand (logxor (* h 31) (sxhash v)) #.(1- (ash 1 62)))))))

(defun %labels-equal (a b)
  (loop (cond ((null a) (return (null b)))
              ((null b) (return nil))
              ((not (equal (pop a) (pop b))) (return nil)))))

(defclass ht-metrics-storage (synchronizable)
  ((table :initform (make-array 8 :initial-element nil) :accessor metrics-storage-table)
   (series :initform (make-array 8 :initial-element nil) :accessor metrics-storage-series)
   ;; %LABELS-HASH of each series, writer side only
   (hashes :initform (make-array 8 :element-type 'fixnum :initial-element 0)
           :accessor metrics-storage-hashes)
   (count :initform 0 :accessor metrics-storage-count)))

(declaim (inline %find-series))
(defun %find-series (table labels)
  (declare (type simple-vector table))
  (let ((mask (1- (length table))))
    (loop for i = (logand (%labels-hash labels) mask) then (logand (1+ i) mask)
          for s = (svref table i)
          do (cond ((null s) (return nil))
                   ((%labels-equal labels (metric-labels s)) (return s))))))

(defun %table-insert (table metric hash)
  (declare (type simple-vector table) (type fixnum hash) (optimize speed))
  (let ((mask (1- (length table))))
    (loop for i of-type fixnum = (logand hash mask) then (logand (1+ i) mask)
          unless (svref table i)
            do #+sbcl (sb-thread:barrier (:write)) ; METRIC is complete before it is visible
               (setf (svref table i) metric)
               (return))))

(defgeneric get-metric (thing labels)
  (:method ((storage ht-metrics-storage) labels)
    (%find-series (metrics-storage-table storage) labels)))

(defgeneric get-metrics (thing)
  (:method ((storage ht-metrics-storage))
    ;; count first: any vector read after it holds that many series
    (let ((n (metrics-storage-count storage)))
      #+sbcl (sb-thread:barrier (:read))
      (let ((series (metrics-storage-series storage)))
        ;; newest first, the order scrapes have always had
        (loop for i from (1- n) downto 0 collect (svref series i))))))

(defun %publish-metric (storage metric)
  ;; caller holds the storage lock
  (let ((n (metrics-storage-count storage))
        (table (metrics-storage-table storage))
        (series (metrics-storage-series storage))
        (hashes (metrics-storage-hashes storage))
        (hash (%labels-hash (metric-labels metric))))
    (declare (type (simple-array fixnum (*)) hashes))
    (when (>= (* 2 (1+ n)) (length table))
      (let ((bigger (make-array (* 2 (length table)) :initial-element nil)))
        (dotimes (i n) (%table-insert bigger (svref series i) (aref hashes i)))
        #+sbcl (sb-thread:barrier (:write))
        (setf table bigger
              (metrics-storage-table storage) bigger)))
    (when (= n (length series))
      (let ((bigger (make-array (* 2 n) :initial-element nil)))
        (replace bigger series)
        #+sbcl (sb-thread:barrier (:write))
        (setf series bigger
              (metrics-storage-series storage) bigger))
      (let ((bigger (make-array (* 2 n) :element-type 'fixnum :initial-element 0)))
        (replace bigger hashes)
        (setf hashes bigger
              (metrics-storage-hashes storage) bigger)))
    (setf (svref series n) metric
          (aref hashes n) hash)
    (%table-insert table metric hash)
    #+sbcl (sb-thread:barrier (:write))
    (setf (metrics-storage-count storage) (1+ n))
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
