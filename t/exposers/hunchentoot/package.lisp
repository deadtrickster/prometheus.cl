(in-package #:cl-user)

(defpackage #:prometheus.exposers.hunchentoot.test
  (:use #:cl #:prove #:alexandria))

(in-package #:prometheus.exposers.hunchentoot.test)

(defmacro with-fresh-registry (&body body)
  `(let ((prom:*default-registry* (make-instance 'prom:registry)))
     ,@body))
