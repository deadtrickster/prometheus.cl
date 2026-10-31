(in-package #:prometheus)

(define-constant +counter-default+ 0)

(defclass counter (metric-family)
  ((shards :initarg :shards :initform (default-shards) :reader counter-shards))
  (:default-initargs :type "counter"))

(defmethod mf-make-metric ((metric counter) labels)
  (make-instance 'counter-metric :labels labels
                                 :cells (make-counter-cells (counter-shards metric))))

(defclass counter-metric (simple-metric)
  ((cells :initarg :cells :reader counter-metric-cells)))

(defmethod metric-value ((counter counter-metric))
  (counter-cells-value (counter-metric-cells counter)))

(defun check-counter-value (value)
  (unless (numberp value)
    (error 'invalid-value-error :value value :reason "value is not a number"))
  (when (< value 0)
    (error 'invalid-value-error :value value :reason "counters can only be incremented by non-negative amounts")))

(defgeneric counter.inc% (counter n labels)
  (:method ((counter counter) n labels)
    (let ((metric (get-metric counter labels)))
      (counter.inc% metric n labels)))
  (:method ((counter counter-metric) n labels)
    (declare (ignore labels))
    #+sbcl (counter-cells-add (counter-metric-cells counter) n)
    #-sbcl (synchronize counter (counter-cells-add (counter-metric-cells counter) n))))

(defun counter.inc (counter &key (value 1) labels)
  (check-counter-value value)
  (counter.inc% counter value labels))

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
