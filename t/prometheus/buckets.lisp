(in-package #:prometheus.test)

(plan 1)

(subtest "BUCKET GENERATORS"
  (subtest "LINEAR"
    (subtest "Errors & Validations"
      (is-error-report (prom:generate-linear-buckets 1 2 0) prom:invalid-value-error "Value 0 is invalid. Reason: buckets count should be positive"))
    (subtest "Generation"
      (is (prom:generate-linear-buckets -15 5 6) '(-15 -10 -5 0 5 10) :test #'equalp)))

  (subtest "EXPONENTIAL"
    (subtest "Errors & Validations"
      (is-error-report (prom:generate-exponential-buckets 1 2 0) prom:invalid-value-error "Value 0 is invalid. Reason: buckets count should be positive")
      (is-error-report (prom:generate-exponential-buckets -1 2 3) prom:invalid-value-error "Value -1 is invalid. Reason: buckets start should be positive")
      (is-error-report (prom:generate-exponential-buckets 1 -2 3) prom:invalid-value-error "Value -2 is invalid. Reason: buckets factor should be greater than 1"))
    (subtest "Generation"
      (is (prom:generate-exponential-buckets 100 1.2d0 3) '(100 120 144) :test #'equalp))))

(finalize)
