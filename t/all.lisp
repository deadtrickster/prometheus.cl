(defpackage #:prometheus.test.all
  (:use #:cl)
  (:export #:run))

(in-package #:prometheus.test.all)

(defun test-systems ()
  ;; resolve (:feature f system) entries
  (loop for dep in (asdf:system-depends-on (asdf:find-system :prometheus.test.all))
        when (atom dep) collect dep
        else when (and (eq (first dep) :feature) (uiop:featurep (second dep)))
               collect (third dep)))

(defun run-all ()
  ;; run every system, do not stop at the first failure
  (every #'identity (mapcar #'prove:run (test-systems))))

(defun run (&key ci)
  (let ((ok (run-all)))
    (when ci
      (uiop:quit (if ok 0 1)))
    ok))
