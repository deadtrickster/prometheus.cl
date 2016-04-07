(in-package :prometheus.sbcl)

(defclass threads-collector (prom:collector)
  ())

(defun make-threads-collector (&key (namespace "") (name "sbcl_threads") (registry prom:*default-registry*))
  (let ((collector (make-instance 'threads-collector :namespace namespace :name name)))
    (when registry
      (prom:register collector registry))
    collector))


(defmethod prom:collect ((tc threads-collector) cb)
  (funcall cb (prom:make-gauge :name (prom:collector-metric-name tc "sbcl_threads")
                               :help "SBCL Threads Count"
                               :value (length (sb-thread:list-all-threads))
                               :registry nil)))
