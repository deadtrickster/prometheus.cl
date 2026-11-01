(in-package #:prometheus)

(define-constant +counter-default+ 0)

(defclass int-counter (counter)
  ()
  (:default-initargs :type "counter"))

(defstruct (int-counter-metric (:include counter-metric) (:copier nil) (:predicate nil)))

(defmethod mf-make-metric ((metric int-counter) labels)
  (make-int-counter-metric :labels labels
                           :cells (make-counter-cells (counter-shards metric))))

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
