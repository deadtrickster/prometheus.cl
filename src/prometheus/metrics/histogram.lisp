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

(defun validate-and-normalize-buckets (buckets)
  (unless (listp buckets)
    (error 'invalid-buckets-error :value buckets :reason "expected LIST"))

  (unless (> (length buckets) 0)
    (error 'invalid-buckets-error :value buckets :reason "must be at least one bucket"))

  (dolist (bound buckets)
    (unless (or (integerp bound)
                (floatp bound))
      (error 'invalid-bucket-bound-error :value bound :reason "bucket bound is not an integer/float")))

  (unless (equalp (sort (copy-seq buckets) #'<) buckets)
    (error 'invalid-buckets-error :value buckets :reason "bounds not sorted"))

  (let ((with+inf (make-array (1+ (length buckets)) :element-type 'number :initial-element #+allegro excl::*infinity-double* #+lispworks #.(read-from-string "10E999") #+sbcl sb-ext:double-float-positive-infinity #+(and ecl ieee-floating-point) EXT:DOUBLE-FLOAT-POSITIVE-INFINITY)))
    (replace with+inf buckets)
    with+inf))

(defmethod validate-args ((mf histogram) &rest initargs &key labels buckets &allow-other-keys)
  (declare (ignore initargs))
  (when (find "le" labels :test #'equal)
    (error 'invalid-label-name-error :name "le" :reason "histogram cannot have a label named \"le\""))
  (list :buckets (validate-and-normalize-buckets buckets)))

(defclass histogram-metric (metric)
  ((value :reader histogram-buckets)
   (sum :initform 0 :reader histogram-sum)))

(defmethod histogram-count ((metric histogram-metric))
  (reduce (lambda (val b)
            (+ val (prom:bucket-count b)))
          (prom:metric-value metric) :initial-value 0))

(defmethod mf-make-metric ((metric histogram) labels)
  (make-instance 'histogram-metric :labels labels
                                   :value (bucket-bounds-to-buckets (histogram-buckets metric))))

(defun check-histogram-value (value)
  (unless (numberp value)
    (error 'invalid-value-error :value value :reason "value is not a number")))

(defgeneric histogram.observe% (histogram value labels)
  (:method ((histogram histogram) value labels)
    (let ((metric (get-metric histogram labels)))
      (histogram.observe% metric value labels)))
  (:method ((metric histogram-metric) value labels)
    (declare (ignore labels))
    #-(or sbcl lispworks)
    (synchronize metric
      (loop for bucket across (metric-value metric)
            do
               (when (<= value (bucket-bound bucket))
                 (incf (bucket-count bucket))
                 (return)))
      (incf (slot-value metric 'sum) value))
    #+(or sbcl lispworks)
    (progn
      (loop for bucket across (metric-value metric)
            do
               (when (<= value (bucket-bound bucket))
                 (cas-incf (bucket-count bucket) 1)
                 (return)))
      (cas-incf (slot-value metric 'sum) value))))

(defun histogram.observe (histogram value &key labels)
  (check-histogram-value value)
  (histogram.observe% histogram value labels))

(defmacro histogram.time (histogram &body body)
  `(timing% (lambda (time)
              (histogram.observe ,histogram time))
     (lambda () ,@body)))

(defun make-histogram (&key name help labels buckets value (registry *default-registry*))
  (check-value-or-labels value labels)
  (let ((histogram (make-instance 'histogram :name name
                                             :help help
                                             :labels labels
                                             :buckets buckets
                                             :registry registry)))
    (when value
      (if (listp value)
          (dolist (v value)
            (histogram.observe histogram v))
          (histogram.observe histogram value)))
    histogram))
