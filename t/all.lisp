(defpackage #:prometheus.test.all
  (:use #:cl)
  (:export #:run))

(in-package #:prometheus.test.all)

(defun run-all ()
  (every (lambda (c)
           (prove:run c))
         (asdf:system-depends-on (asdf:find-system :prometheus.test.all))))

(defun run-all-ci ()
  (let ((result))
    (coveralls:with-coveralls (:exclude '("t" "examples" "benchmark"))
      (setf result (run-all)))
    (unless result
      (uiop:quit 1))))

(defun run (&key ci)
  (if ci
      (run-all-ci)
      (run-all)))
