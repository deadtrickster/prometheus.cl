(in-package #:cl-user)

(defpackage #:prometheus.test.support
  (:use #:cl #:alexandria #:prove)
  (:export #:error-class-exists
           #:is-error-report
           #:error-report-test
           #:with-fresh-registry))
