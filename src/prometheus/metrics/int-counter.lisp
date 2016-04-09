(in-package #:prometheus)

(define-constant +counter-default+ 0)

(defclass int-counter (counter)
  ()
  (:default-initargs :type "counter"))

(defmethod mf-make-metric ((metric int-counter) labels)
  (make-instance 'int-counter-metric :labels labels))

(defstruct int-counter-storage
  (value 0 :type (unsigned-byte 64)))

(defclass int-counter-metric (simple-metric)
  ((value :initform (make-int-counter-storage))))

(defmethod metric-value ((m int-counter-metric))
  #-sbcl
  (call-next-method)
  #+sbcl
  (int-counter-storage-value (slot-value m 'value)))

(defun check-int-counter-value (value)
  )

(defmethod counter.inc% ((int-counter int-counter-metric) n labels)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (unless (typep n '(unsigned-byte 64))
    (error 'invalid-value-error :value n :reason "value is not an uint64"))
  #-sbcl
  (call-next-method)
  #+sbcl
  (sb-ext:atomic-incf (int-counter-storage-value (slot-value int-counter 'value)) n))

(defmethod counter.reset ((int-counter int-counter-metric) &key labels)
  (declare (ignore labels))
  (synchronize int-counter
    (setf (slot-value int-counter 'value) (make-int-counter-storage))))

(defun make-int-counter (&key name help labels value (registry *default-registry*))
  (check-value-or-labels value labels)
  (let ((int-counter (make-instance 'int-counter :name name
                                                 :help help
                                                 :labels labels
                                                 :registry registry)))
    (when value
      (counter.inc int-counter :value value))
    int-counter))
