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

(defun validate-label-names (labels)
  (every (lambda (label)
           (and (check-label-name-is-string label)
                (check-label-name-not-reserved label)
                (check-label-name-does-not-start-with__ label)))
         labels))

(defun validate-label-values (values)
  (every #'stringp values))
