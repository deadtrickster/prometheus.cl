(in-package #:prometheus)

(define-condition base-error (error)
  ())

(define-condition invalid-value-error (base-error)
  ((value :initarg :value)
   (reason :initarg :reason))
  (:report (lambda (error stream)
             (format stream "Value ~s is invalid. Reason: ~a" (slot-value error 'value) (slot-value error 'reason)))))

(define-condition invalid-label-name-error (base-error)
  ((name :initarg :name)
   (reason :initarg :reason))
  (:report (lambda (error stream)
             (format stream "Label name ~s is invalid. Reason: ~a" (slot-value error 'name) (slot-value error 'reason)))))

(define-condition invalid-label-value-error (invalid-value-error)
  ()
  (:report (lambda (error stream)
             (format stream "Label value ~s is invalid. Reason: ~a" (slot-value error 'value) (slot-value error 'reason)))))

(define-condition invalid-label-count-error (base-error)
  ((actual :initarg :actual)
   (expected :initarg :expected))
  (:report (lambda (error stream)
             (format stream "Invalid label count. Got ~s, expected ~a" (slot-value error 'actual) (slot-value error 'expected)))))

(define-condition invalid-labels-error (base-error)
  ((actual :initarg :actual)
   (expected :initarg :expected))
  (:report (lambda (error stream)
             (format stream "Invalid labels. Got ~s (type: ~a), expected ~a" (slot-value error 'actual) (type-of (slot-value error 'actual)) (slot-value error 'expected)))))

(define-condition invalid-metric-name-error (base-error)
  ((name :initarg :name)
   (reason :initarg :reason))
  (:report (lambda (error stream)
             (format stream "Metric name ~s is invalid. Reason: ~a" (slot-value error 'name) (slot-value error 'reason)))))

(define-condition invalid-buckets-error (invalid-value-error)
  ()
  (:report (lambda (error stream)
             (format stream "Invalid buckets. Got ~s (type: ~a), reason: ~a" (slot-value error 'value) (type-of (slot-value error 'value)) (slot-value error 'reason)))))

(define-condition invalid-bucket-bound-error (invalid-value-error)
  ()
  (:report (lambda (error stream)
             (format stream "Bucket bound ~s is invalid. Reason: ~a" (slot-value error 'value) (slot-value error 'reason)))))
