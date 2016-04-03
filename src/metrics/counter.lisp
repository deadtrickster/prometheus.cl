(in-package #:prometheus)

(define-constant +counter-default+ 0)

(defclass counter (metric-family)
  ()
  (:default-initargs :type "counter"))

(defmethod mf-make-metric ((metric counter) labels)
  (make-instance 'counter-metric :labels labels))

(defclass counter-metric (simple-metric)
  ((value :initform +counter-default+)))

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
  (counter.inc% counter value labels))

(defgeneric counter.reset (counter &key labels)
  (:method ((counter counter) &key labels)
    (synchronize counter
      (let ((metric (get-metric counter labels)))
        (reset metric))))
  (:method ((counter counter-metric) &key labels)
    (declare (ignore labels))
    (synchronize counter
      (setf (slot-value counter 'value) +counter-default+))))

(defun make-counter (&key name help labels value (registry *default-registry*))
  (assert (not (and labels value)) nil "Can only specify at most one of value and labels.")
  (let ((counter (make-instance 'counter :name name
                                         :help help
                                         :labels labels)))
    (when value
      (counter.inc counter :value value))
    (when registry
      (register counter registry))
    counter))
