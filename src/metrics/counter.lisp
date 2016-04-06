(in-package #:prometheus)

(define-constant +counter-default+ 0)

(defclass counter (metric-family)
  ()
  (:default-initargs :type "counter"))

(defmethod mf-make-metric ((metric counter) labels)
  (make-instance 'counter-metric :labels labels))

(defclass counter-metric (simple-metric)
  ((value :initform +counter-default+)))

(defun check-counter-value (value)
  (unless (numberp value)
    (error 'invalid-value-error :value value :reason "value is not a number"))
  (when (< value 0)
    (error 'invalid-value-error :value value :reason "counters can only be incremented by non-negative amounts")))

(defgeneric counter.inc% (counter n labels)
  (:method ((counter counter) n labels)
    (synchronize counter
      (let ((metric (get-metric counter labels)))
        (counter.inc% metric n labels))))
  (:method ((counter counter-metric) n labels)
    (declare (ignore labels))
    (synchronize counter
      (incf (slot-value counter 'value) n))))

(defun counter.inc (counter &key (value 1) labels)
  (check-counter-value value)
  (counter.inc% counter value labels))

(defgeneric counter.reset (counter &key labels)
  (:method ((counter counter) &key labels)
    (synchronize counter
      (let ((metric (get-metric counter labels)))
        (counter.reset metric))))
  (:method ((counter counter-metric) &key labels)
    (declare (ignore labels))
    (synchronize counter
      (setf (slot-value counter 'value) +counter-default+))))

(defun make-counter (&key name help labels value (registry *default-registry*))
  (assert (not (and labels value)) nil 'invalid-value-error :value value :reason "can only specify at most one of value and labels")
  (let ((counter (make-instance 'counter :name name
                                         :help help
                                         :labels labels)))
    (when value
      (counter.inc counter :value value))
    (when registry
      (register counter registry))
    counter))
