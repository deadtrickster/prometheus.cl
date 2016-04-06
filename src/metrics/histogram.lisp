(in-package #:prometheus)

(defstruct bucket
  (bound)
  (count 0))

(defun bucket-bounds-to-buckets (buckets)
  (let ((ret (make-sequence 'simple-vector (length buckets))))
    (loop for i from 0 below (length buckets)
          do
             (setf (aref ret i)
                   (make-bucket :bound (aref buckets i))))
    ret))

(defclass histogram (metric-family)
  ((buckets :initarg :buckets :reader histogram-buckets))
  (:default-initargs :type "histogram"))

(defmethod validate-args ((mf histogram) &rest initargs &key labels &allow-other-keys)
  (declare (ignore initargs))
  (when (find "le" labels :test #'equal)
    (error 'invalid-label-name-error :name "le" :reason "histogram cannot have a label named \"le\"")))

(defclass histogram-metric (metric)
  ((sum :initform 0 :reader histogram-sum)))

(defmethod mf-make-metric ((metric histogram) labels)
  (make-instance 'histogram-metric :labels labels
                                   :value (bucket-bounds-to-buckets (histogram-buckets metric))))

(defgeneric histogram.observe (histogram value &key labels)
  (:method ((histogram histogram) value &key labels)
    (synchronize histogram
      (let ((metric (get-metric histogram labels)))
        (histogram.observe metric value))))
  (:method ((metric histogram-metric) value &key labels)
    (declare (ignore labels))
    (synchronize metric
      (loop for bucket across (metric-value metric)
            do
               (when (<= value (bucket-bound bucket))
                 (incf (bucket-count bucket))
                 (return)))
      (incf (slot-value metric 'sum) value))))

(defun validate-buckets (buckets)
  (assert (every (lambda (v)
                   (or (integerp v)
                       (floatp v)))
                 buckets) nil "Buckets bounds must be integers/floats")
  (assert (> (length buckets) 0) nil "Must be at least one bucket")
  (assert (equalp (sort (copy-seq buckets) #'<) buckets) nil "Buckets not sorted")
  (let ((with+inf (make-array (1+ (length buckets)) :element-type 'number :initial-element #+sbcl sb-ext:double-float-positive-infinity #+(and ecl ieee-floating-point) EXT:DOUBLE-FLOAT-POSITIVE-INFINITY)))
    (replace with+inf buckets)
    with+inf))

(defun make-histogram (&key name help labels buckets value (registry *default-registry*))
  (check-value-or-labels value labels)
  (let ((histogram (make-instance 'histogram :name name
                                             :help help
                                             :labels labels
                                             :buckets (validate-buckets buckets))))
    (when value
      (dolist (v value)
        (histogram.observe histogram v)))
    (when registry
      (register histogram registry))
    histogram))

(defmacro histogram.time (histogram &body body)
  `(timing% (lambda (time)
              (histogram.observe ,histogram time))
            (lambda () ,@body)))
