(in-package #:cl-user)

(defpackage #:prometheus.formats.text.test
  (:use :cl :prove))

(in-package #:prometheus.formats.text.test)

(defmacro with-fresh-registry (&body body)
  `(let ((prom:*default-registry* (make-instance 'prom:registry)))
     ,@body))
