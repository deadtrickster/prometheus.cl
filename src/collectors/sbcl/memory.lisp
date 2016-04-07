(in-package #:prometheus.sbcl)

(defclass memory-collector (prom:collector)
  ((cutoff :initarg :cutoff :reader memory-collector-cutoff)))

(defun make-memory-collector (&key (namespace "") (cutoff 0.05f0) (name "sbcl_memory_collector") (registry prom:*default-registry*))
  (let ((collector (make-instance 'memory-collector :namespace namespace
                                                    :name name
                                                    :cutoff cutoff)))
    (when registry
      (prom:register collector registry))
    collector))

(defun object-type-name-to-label (name)
  (string-downcase (symbol-name name)))

(defmethod prom:collect ((mc memory-collector) cb)
  (funcall cb (prom:make-gauge :name (prom:collector-metric-name mc "sbcl_read_only_bytes")
                               :help "SBCL Read-only space usage"
                               :value (sb-kernel::read-only-space-usage)
                               :registry nil))
  (funcall cb (prom:make-gauge :name (prom:collector-metric-name mc "sbcl_static_bytes")
                               :help "SBCL Static space usage"
                               :value (sb-kernel::static-space-usage)
                               :registry nil))
  (let ((bytes-gauge (prom:make-gauge :name (prom:collector-metric-name mc "sbcl_dynamic_bytes")
                                      :help "SBCL Dynamic space usage"
                                      :labels '("object_type")
                                      :registry nil))
        (objects-gauge (prom:make-gauge :name (prom:collector-metric-name mc "sbcl_dynamic_objects")
                                      :help "SBCL Dynamic space objects"
                                      :labels '("object_type")
                                      :registry nil)))
    (get-sbcl-dynamic-memory-breakdown (memory-collector-cutoff mc)
                                       (lambda (name bytes objects)
                                         (let ((labels (list (object-type-name-to-label name))))
                                           (prom:gauge.set bytes-gauge bytes :labels labels)
                                           (prom:gauge.set objects-gauge objects :labels labels))))
    (funcall cb bytes-gauge)
    (funcall cb objects-gauge)))

(defun get-sbcl-dynamic-memory-breakdown (cutoff cb)
  (let ((full-breakdown (sb-vm::type-breakdown :dynamic)))
    ;; note: full-breakdown is sorted ASC by bytes
    ;; some code from room.lisp
    (let* ((total-bytes (reduce #'+ (mapcar #'first full-breakdown)))
           (total-objects (reduce #'+ (mapcar #'second full-breakdown)))
           (cutoff-point (if cutoff
                             (truncate (* (float total-bytes) cutoff))
                             0))
           (reported-bytes 0)
           (reported-objects 0))
      (append
       (loop for (bytes objects name) in full-breakdown
             if (<= bytes cutoff-point) do
                (return)
             else do
                (incf reported-bytes bytes)
                (incf reported-objects objects)
                (funcall cb name bytes objects))
       (funcall cb :other (- total-bytes reported-bytes) (- total-objects reported-objects))))))
