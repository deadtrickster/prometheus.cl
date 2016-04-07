(in-package #:cl-user)

(defpackage #:prometheus.collectors.sbcl.test
  (:use #:cl #:prove #:alexandria))

(in-package #:prometheus.collectors.sbcl.test)

(defmacro with-fresh-registry (&body body)
  `(let ((prom:*default-registry* (make-instance 'prom:registry)))
     ,@body))
