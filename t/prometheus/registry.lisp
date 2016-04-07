(in-package #:prometheus.test)

(plan 1)

(defclass test-collector (prom:collector)
  ())

(defmethod prom:collect ((collectable test-collector) cb)
  (funcall cb "called"))

(subtest "Registry tests"

  (subtest "Errors & Validations"
    (with-fresh-registry
      (let ((c (make-instance 'prom:collector :name "qwe")))
        (prom:register c)
        (is-error (prom:register (make-instance 'prom:collector :name "qwe")) 'prom:collectable-already-registered-error "Can't register another collector with already registered name")
        (ok (prom:register c) "Can \"register\" the same collector under same name"))))

  (subtest "REGISTER & UNREGISTER & REGISTEREDP"
    (with-fresh-registry
      (let ((c (make-instance 'prom:collector :name "qwe")))
        (prom:register c)
        (ok (prom:registeredp c prom:*default-registry*) "Collector registered")
        (prom:unregister c)
        (ok (not (prom:registeredp c prom:*default-registry*)) "Collector unregistered")
        (prom:register c)
        (is (prom:registeredp "qwe") c "REGISTEREDP can accept collectable name")
        (prom:unregister "qwe")
        (ok (not (prom:registeredp c prom:*default-registry*)) "UNREGISTER can accept collectable name"))))

  (subtest "COLLECT"
    (with-fresh-registry
      (let ((c (make-instance 'test-collector :name "qwe"))
            (cv))
        (prom:register c)
        (prom:collect prom:*default-registry* (lambda (v)
                                                (setf cv v)))
        (is cv "called" "Collect actually calls callback with collectables")))))

(finalize)
