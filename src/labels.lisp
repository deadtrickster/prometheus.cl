(in-package #:prometheus)

(defun check-label-name-is-string (label)
  (unless (stringp label)
    (error 'invalid-label-name-error :name label  :reason "label name is not a string")))

(defun check-label-name-not-reserved (label)
  (unless (and (not (equal label "job"))
               (not (equal label "instance")))
    (error 'invalid-label-name-error :name label :reason "label name is reserved")))

(defun check-label-name-does-not-start-with__ (label)
  (when (and (> (length label) 1)
             (eql #\_ (aref label 0))
             (eql #\_ (aref label 1)))
    (error 'invalid-label-name-error :name label :reason "label name starts with __")))

(defun check-label-name-regex (label)
  (unless (equal label (ppcre:scan-to-strings "[a-zA-Z_][a-zA-Z0-9_]*" label))
    (error 'invalid-label-name-error :name label :reason "label name doesn't match regex [a-zA-Z_][a-zA-Z0-9_]*")))

(defun check-label-name (label)
  (check-label-name-is-string label)
  (check-label-name-not-reserved label)
  (check-label-name-does-not-start-with__ label)
  (check-label-name-regex label))

(defun check-label-names (labels)
  (dolist (label labels)
    (check-label-name label))
  labels)

(defun check-label-values (values)
  (dolist (value values)
    (unless (stringp value)
      (error 'invalid-label-value-error :value value :reason "label value is not a string"))))
