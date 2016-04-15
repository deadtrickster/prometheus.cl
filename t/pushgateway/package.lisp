(in-package #:cl-user)

(defpackage #:prometheus.pushgateway.test
  (:use #:cl #:prove #:alexandria))

(in-package #:prometheus.pushgateway.test)

(defmacro with-fresh-registry (&body body)
  `(let ((prom:*default-registry* (make-instance 'prom:registry)))
     ,@body))
