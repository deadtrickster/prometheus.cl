(in-package #:cl-user)

(defpackage #:prometheus.process
  (:use #:cl #:alexandria #:split-sequence)
  (:nicknames #:prom.process)
  (:export #:make-process-collector))
