(in-package #:prometheus)

(defun try-to-maintain-integer-bounds (bound)
  (let ((boundf (floor bound)))
    (if (= bound boundf)
        boundf
        bound)))

(defun generate-linear-buckets (start step count)
  (assert (> count 1) (count) 'invalid-value-error :value count :reason "buckets count should be positive")

  (loop repeat count
        collect (try-to-maintain-integer-bounds (prog1 start
                                                 (incf start step)))))

(defun generate-exponential-buckets (start factor count)
  (assert (> count 1) (count) 'invalid-value-error :value count :reason "buckets count should be greater than 1")
  (assert (> start 0) (start) 'invalid-value-error :value start :reason "buckets start should be positive")
  (assert (> factor 1) (factor) 'invalid-value-error :value factor :reason "buckets factor should be greater than 1")

  (loop repeat count
        collect (try-to-maintain-integer-bounds (prog1 start
                                                 (setf start (* start factor))))))
