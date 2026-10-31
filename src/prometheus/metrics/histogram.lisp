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

(defun normalize-bucket-bound (bound)
  ;; 0.005f0 -> 0.005d0, the value the caller wrote. Coercing directly gives
  ;; 0.004999999888241291d0, which misfiles observations of exactly 0.005d0.
  (if (and (floatp bound) (not (typep bound 'double-float)))
      (coerce (rationalize bound) 'double-float)
      bound))

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

  (let ((with+inf (make-array (1+ (length buckets)) :element-type 'number :initial-element #+allegro excl::*infinity-double* #+lispworks 1D++0 #+sbcl sb-ext:double-float-positive-infinity #+(and ecl ieee-floating-point) EXT:DOUBLE-FLOAT-POSITIVE-INFINITY)))
    (replace with+inf (mapcar #'normalize-bucket-bound buckets))
    with+inf))

(defmethod validate-args ((mf histogram) &rest initargs &key labels buckets &allow-other-keys)
  (declare (ignore initargs))
  ;; any case: the text format lowercases label names
  (when-let ((label (find "le" labels :test #'string-equal)))
    (error 'invalid-label-name-error :name label :reason "histogram cannot have a label named \"le\""))
  (list :buckets (validate-and-normalize-buckets buckets)))

(defstruct (histogram-metric (:include metric) (:copier nil) (:predicate nil))
  (buckets #() :type simple-vector :read-only t)
  (sum 0))

(defmethod histogram-buckets ((metric histogram-metric))
  (histogram-metric-buckets metric))

(defmethod metric-value ((metric histogram-metric))
  (histogram-metric-buckets metric))

(defgeneric histogram-sum (metric)
  (:method ((metric histogram-metric))
    (histogram-metric-sum metric)))

(defgeneric histogram-count (metric)
  (:method ((metric histogram-metric))
    (loop for bucket across (histogram-metric-buckets metric)
          sum (bucket-count bucket))))

(defmethod mf-make-metric ((metric histogram) labels)
  (make-histogram-metric :labels labels
                         :buckets (bucket-bounds-to-buckets (histogram-buckets metric))))

(defun histogram.observe (histogram value &key labels)
  "Observe VALUE on HISTOGRAM, a family or a series from GET-METRIC. Passing
the series skips the label lookup."
  (unless (numberp value)
    (error 'invalid-value-error :value value :reason "value is not a number"))
  (let ((series (if (typep histogram 'histogram-metric) histogram (get-metric histogram labels))))
    #-(or sbcl lispworks)
    (synchronize series
      (loop for bucket across (histogram-metric-buckets series)
            when (<= value (bucket-bound bucket))
              do (incf (bucket-count bucket))
                 (return))
      (incf (histogram-metric-sum series) value))
    #+(or sbcl lispworks)
    (progn
      (loop for bucket across (histogram-metric-buckets series)
            when (<= value (bucket-bound bucket))
              do (cas-incf (bucket-count bucket) 1)
                 (return))
      (cas-incf (histogram-metric-sum series) value)))
  nil)

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
