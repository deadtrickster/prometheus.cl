(in-package #:prometheus)

(define-constant +counter-default+ 0)

(defclass int-counter (counter)
  ()
  (:default-initargs :type "counter"))

(defmethod mf-make-metric ((metric int-counter) labels)
  (make-instance 'int-counter-metric :labels labels
                                     :cells (make-counter-cells (counter-shards metric))))

(defclass int-counter-metric (counter-metric)
  ())

(defmethod counter.inc% ((int-counter int-counter-metric) n labels)
  (declare (ignore labels))
  (unless (typep n '(unsigned-byte 64))
    (error 'invalid-value-error :value n :reason "value is not an uint64"))
  ;; uint64 semantics: wraps instead of carrying into a double
  #+sbcl (counter-cells-add (counter-metric-cells int-counter) n nil)
  #-sbcl (synchronize int-counter (counter-cells-add (counter-metric-cells int-counter) n nil)))

(defun make-int-counter (&key name help labels value (shards (default-shards)) (registry *default-registry*))
  (check-value-or-labels value labels)
  (check-shards shards)
  (let ((int-counter (make-instance 'int-counter :name name
                                                 :help help
                                                 :labels labels
                                                 :shards shards
                                                 :registry registry)))
    (when value
      (counter.inc int-counter :value value))
    int-counter))
