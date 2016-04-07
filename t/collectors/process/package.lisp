(in-package #:cl-user)

(defpackage #:prometheus.collectors.process.test
  (:use #:cl #:prove #:alexandria))

(in-package #:prometheus.collectors.process.test)

(defmacro with-fresh-registry (&body body)
  `(let ((prom:*default-registry* (make-instance 'prom:registry)))
     ,@body))
