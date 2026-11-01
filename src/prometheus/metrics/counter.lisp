(in-package #:prometheus)

(define-constant +counter-default+ 0)

(defclass counter (metric-family)
  ((shards :initarg :shards :initform (default-shards) :reader counter-shards))
  (:default-initargs :type "counter"))

(defmethod mf-make-metric ((metric counter) labels)
  (make-counter-metric :labels labels
                       :cells (make-counter-cells (counter-shards metric))))

(defstruct (counter-metric (:include simple-metric) (:copier nil) (:predicate nil))
  (cells #() :type simple-vector :read-only t))

(defmethod metric-value ((counter counter-metric))
  (counter-cells-value (counter-metric-cells counter)))

(declaim (inline check-counter-value))
(defun check-counter-value (value)
  (typecase value
    (fixnum (when (< value 0) (%negative-counter-value value)))
    (real (when (minusp value) (%negative-counter-value value)))
    (t (error 'invalid-value-error :value value :reason "value is not a number"))))

(defun %negative-counter-value (value)
  (error 'invalid-value-error :value value :reason "counters can only be incremented by non-negative amounts"))

(defun %counter.inc (counter value labels)
  (declare (optimize speed))
  (check-counter-value value)
  (let ((series (if (typep counter 'counter-metric) counter (get-metric counter labels))))
    (declare (type counter-metric series))
    (when (and (typep series 'int-counter-metric)
               (not (typep value '(unsigned-byte 64))))
      (error 'invalid-value-error :value value :reason "value is not an uint64"))
    #+sbcl (counter-cells-add (counter-metric-cells series) value)
    #-sbcl (synchronize series (counter-cells-add (counter-metric-cells series) value))
    nil))

(defun counter.inc (counter &key (value 1) labels)
  "Increment COUNTER, a family or a series from GET-METRIC. Passing the series
skips the label lookup."
  (%counter.inc counter value labels))

;; skip keyword parsing at call sites
(define-compiler-macro counter.inc (counter &key (value 1) labels)
  `(%counter.inc ,counter ,value ,labels))

(defgeneric counter.reset (counter &key labels)
  (:method ((counter counter) &key labels)
    (let ((metric (get-metric counter labels)))
      (counter.reset metric)))
  (:method ((counter counter-metric) &key labels)
    (declare (ignore labels))
    (synchronize counter
      (counter-cells-reset (counter-metric-cells counter)))))

(defun make-counter (&key name help labels value (shards (default-shards)) (registry *default-registry*))
  (check-value-or-labels value labels)
  (check-shards shards)
  (let ((counter (make-instance 'counter :name name
                                         :help help
                                         :labels labels
                                         :shards shards
                                         :registry registry)))
    (when value
      (counter.inc counter :value value))
    counter))
