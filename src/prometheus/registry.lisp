(in-package #:prometheus)

(defclass registry (synchronizable)
  ((hash-table :initform (make-hash-table :test #'equal) :reader registry-hash-table)))

(defparameter *default-registry* (make-instance 'registry))

(defmethod print-object ((r registry) stream)
  (print-unreadable-object (r stream :identity t)
    (format stream "Prometheus ~:[registry~;default registry~]. Registered ~a collectables" (eq *default-registry* r) (hash-table-count (registry-hash-table r)))))

(defgeneric register% (registry collectable)
  (:method ((registry registry) (collectable collectable))
    (synchronize registry
      (when-let ((c (gethash (collectable-name collectable) (registry-hash-table registry))))
        (unless (eq c collectable)
          (error 'collectable-already-registered-error :collectable collectable :registry registry :rname (collectable-name collectable))))
      (setf (gethash (collectable-name collectable) (registry-hash-table registry)) collectable))))

(defgeneric unregister% (registry collectable)
  (:method ((registry registry) (collectable collectable))
    (unregister% registry (collectable-name collectable) ))
  (:method ((registry registry) (collectable-name string))
    (remhash collectable-name (registry-hash-table registry))))

(defgeneric registered-p% (register collectable-designator)
  (:method ((registry registry) (collectable collectable))
    (when (eq collectable (registered-p% registry (collectable-name collectable)))
      collectable))
  (:method ((registry registry) (collectable-name string))
    (gethash collectable-name (registry-hash-table registry))))

(defmethod collect ((registry registry) cb)
  (with-hash-table-iterator (next (registry-hash-table registry))
    (loop
      (multiple-value-bind (more? key collectable) (next)
        (declare (ignore key))
        (unless more?
          (return))
        (collect collectable cb)))))

(defun register (collectable &optional (registry *default-registry*))
  (register% registry collectable))

(defun unregister (collectable &optional (registry *default-registry*))
  (unregister% registry collectable))

(defun registeredp (collectable &optional (register *default-registry*))
  (registered-p% register collectable))
