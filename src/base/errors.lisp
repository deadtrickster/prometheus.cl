(in-package #:prometheus)

(define-condition base-error (error)
  ())

(define-condition invalid-label-name-error (base-error)
  ((name :initarg :name)
   (reason :initarg :reason))
  (:report (lambda (error stream)
             (format stream "Label name ~s is invalid. Reason: ~a" (slot-value error 'name) (slot-value error 'reason)))))

(define-condition invalid-label-value-error (base-error)
  ((value :initarg :value)
   (reason :initarg :reason))
  (:report (lambda (error stream)
             (format stream "Label value ~s is invalid. Reason: ~a" (slot-value error 'value) (slot-value error 'reason)))))

(define-condition invalid-metric-name-error (base-error)
  ((name :initarg :name)
   (reason :initarg :reason))
  (:report (lambda (error stream)
             (format stream "Metric name ~s is invalid. Reason: ~a" (slot-value error 'name) (slot-value error 'reason)))))
