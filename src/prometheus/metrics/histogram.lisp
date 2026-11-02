(in-package #:prometheus)

(defstruct bucket
  (bound)
  (count 0))

(defclass histogram (metric-family)
  ((buckets :initarg :buckets :reader histogram-buckets)
   ;; the same bounds as doubles, for the observe search
   (double-bounds :initarg :double-bounds :reader histogram-double-bounds)
   (shards :initarg :shards :initform (default-shards) :reader histogram-shards))
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

  (let ((with+inf (make-array (1+ (length buckets)) :element-type 'number :initial-element +double-positive-infinity+)))
    (replace with+inf (mapcar #'normalize-bucket-bound buckets))
    with+inf))

(defmethod validate-args ((mf histogram) &rest initargs &key labels buckets &allow-other-keys)
  (declare (ignore initargs))
  ;; any case: the text format lowercases label names
  (when-let ((label (find "le" labels :test #'string-equal)))
    (error 'invalid-label-name-error :name label :reason "histogram cannot have a label named \"le\""))
  (let ((buckets (validate-and-normalize-buckets buckets)))
    (list :buckets buckets
          :double-bounds (map '(simple-array double-float (*))
                              (lambda (b) (coerce b 'double-float))
                              buckets))))

(defstruct (histogram-metric (:include metric) (:copier nil) (:predicate nil))
  (bounds nil :type (simple-array double-float (*)) :read-only t)
  (le-bounds nil :type simple-vector :read-only t)
  ;; per shard a stripe: the sum (integer and double), then bucket counts
  (slab (make-slab 1) :type slab :read-only t))

(defmethod mf-make-metric ((metric histogram) labels)
  (let ((shards #+sbcl (histogram-shards metric) #-sbcl 1)
        (n (length (histogram-buckets metric))))
    (make-histogram-metric
     :labels labels
     :bounds (histogram-double-bounds metric)
     :le-bounds (coerce (histogram-buckets metric) 'simple-vector)
     :slab (make-slab shards n))))

(defmethod histogram-buckets ((metric histogram-metric))
  "Fresh BUCKET structs with counts summed over shards."
  (let ((le (histogram-metric-le-bounds metric))
        (slab (histogram-metric-slab metric)))
    (coerce (loop for i below (length le)
                  collect (make-bucket :bound (svref le i) :count (slab-extra-sum slab i)))
            'simple-vector)))

(defmethod metric-value ((metric histogram-metric))
  (histogram-buckets metric))

(defgeneric histogram-sum (metric)
  (:method ((metric histogram-metric))
    (slab-value (histogram-metric-slab metric))))

(defgeneric histogram-count (metric)
  (:method ((metric histogram-metric))
    (loop for i below (length (histogram-metric-le-bounds metric))
          sum (slab-extra-sum (histogram-metric-slab metric) i))))

(declaim (inline %bucket-index))
(defun %bucket-index (bounds d)
  "First I with D <= BOUNDS[I]. The last bound is +Inf. D is not NaN."
  (declare (type (simple-array double-float (*)) bounds) (type double-float d))
  (let ((lo 0) (hi (1- (length bounds))))
    (declare (type fixnum lo hi))
    (loop while (< lo hi)
          do (let ((mid (ash (+ lo hi) -1)))
               (if (<= d (aref bounds mid))
                   (setf hi mid)
                   (setf lo (1+ mid)))))
    lo))

(defun %histogram.observe (histogram value labels)
  (declare (optimize speed))
  (unless (realp value)
    (error 'invalid-value-error :value value :reason "value is not a number"))
  (let* ((series (if (typep histogram 'histogram-metric) histogram (get-metric histogram labels)))
         (slab (histogram-metric-slab series))
         (base (%slab-current-base slab))
         ;; generic COERCE returns a boxed double
         (d (typecase value
              (double-float value)
              (fixnum (coerce value 'double-float))
              (single-float (coerce value 'double-float))
              (t (coerce value 'double-float))))
         (index (if (%nan-p d)
                    (%nan-value value)
                    (%bucket-index (histogram-metric-bounds series) d))))
    (declare (type histogram-metric series) (type double-float d))
    (declare (type fixnum base index))
    #+sbcl
    (progn
      (sb-ext:atomic-incf (aref slab (+ base 2 index)))
      (%slab-add slab base value))
    #-sbcl
    (synchronize series
      (incf (aref slab (+ base 2 index)))
      (%slab-add slab base value)))
  nil)

(defun histogram.observe (histogram value &key labels)
  "Observe VALUE on HISTOGRAM, a family or a series from GET-METRIC. Passing
the series skips the label lookup."
  (%histogram.observe histogram value labels))

;; skip keyword parsing at call sites
(define-compiler-macro histogram.observe (histogram value &key labels)
  `(%histogram.observe ,histogram ,value ,labels))

(defmacro histogram.time (histogram &body body)
  `(timing% (lambda (time)
              (histogram.observe ,histogram time))
     (lambda () ,@body)))

(defun make-histogram (&key name help labels buckets value (shards (default-shards)) (registry *default-registry*))
  (check-value-or-labels value labels)
  (check-shards shards)
  (let ((histogram (make-instance 'histogram :name name
                                             :help help
                                             :labels labels
                                             :buckets buckets
                                             :shards shards
                                             :registry registry)))
    (when value
      (if (listp value)
          (dolist (v value)
            (histogram.observe histogram v))
          (histogram.observe histogram value)))
    histogram))
