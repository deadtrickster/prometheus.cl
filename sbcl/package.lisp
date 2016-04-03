(in-package #:cl-user)

(defpackage #:prometheus.sbcl
  (:use #:cl #:alexandria)
  (:nicknames #:prom.sbcl)
  (:export #:make-threads-collector
           #:make-memory-collector))
