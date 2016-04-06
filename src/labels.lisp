(in-package #:prometheus)

(defun check-label-name-is-string (label)
  (unless (stringp label)
    (error "Label ~a is not string" label)))

(defun check-label-name-not-reserved (label)
  (unless (and (not (equal label "job"))
               (not (equal label "instance")))
    (error "Label ~a is reserved" label)))

(defun check-label-name-does-not-start-with__ (label)
  (when (and (eql #\_ (aref label 0))
             (eql #\_ (aref label 1)))
    (error "Label ~a starts with __" label)))

(defun check-label-name-regex (label)
  (unless (equal label (ppcre:scan-to-strings "[a-zA-Z_][a-zA-Z0-9_]*" label))
    (error "Label ~a doesn't match regex [a-zA-Z_][a-zA-Z0-9_]*" label)))

(defun check-labels-names (labels)
  (dolist (label labels)
    (check-label-name-is-string label)
    (check-label-name-not-reserved label)
    (check-label-name-does-not-start-with__ label)
    (check-label-name-regex label))
  labels)

(defun validate-label-values (values)
  (every #'stringp values))
