(in-package #:prometheus.test)

(plan 1)

(subtest "Errors"

  (subtest "Base Error"
    (error-class-exists prom:base-error error))

  (subtest "Invalid Value Error"
    (error-class-exists prom:invalid-value-error)

    (error-report-test prom:invalid-value-error ((:value -1 :reason "counters can only be incremented by non-negative amounts")
                                                 "Value -1 is invalid. Reason: counters can only be incremented by non-negative amounts")))


  (subtest "Invalid Label Name Error"
    (error-class-exists prom:invalid-label-name-error)

    (error-report-test prom:invalid-label-name-error ((:name 123 :reason "label name is not a string")
                                                      "Label name 123 is invalid. Reason: label name is not a string")))


  (subtest "Invalid Label Value Error"
    (error-class-exists prom:invalid-label-value-error prom:invalid-value-error)

    (error-report-test prom:invalid-label-value-error ((:value 123 :reason "label value is not a string")
                                                       "Label value 123 is invalid. Reason: label value is not a string")))

  (subtest "Invalid Label Count Error"
    (error-class-exists prom:invalid-label-count-error)

    (error-report-test prom:invalid-label-count-error ((:actual 123 :expected 12)
                                                       "Invalid label count. Got 123, expected 12")))

  (subtest "Invalid Labels Error"
    (error-class-exists prom:invalid-labels-error)
    (error-report-test prom:invalid-labels-error ((:actual #(1 2 3) :expected 'list)
                                                  "Invalid labels. Got #(1 2 3) (type: (SIMPLE-VECTOR 3)), expected LIST")))

  (subtest "Invalid Metric Name Error"
    (error-class-exists prom:invalid-metric-name-error)

    (error-report-test prom:invalid-metric-name-error ((:name 123 :reason "metric name is not a string")
                                                       "Metric name 123 is invalid. Reason: metric name is not a string")))

  (subtest "Invalid Buckets Error"
    (error-class-exists prom:invalid-buckets-error)
    (error-report-test prom:invalid-buckets-error ((:value #(1 2 3) :reason "expected LIST")
                                                   "Invalid buckets. Got #(1 2 3) (type: (SIMPLE-VECTOR 3)), reason: expected LIST")))


  (subtest "Invalid Bucket Bound Error"
    (error-class-exists prom:invalid-bucket-bound-error prom:invalid-value-error)

    (error-report-test prom:invalid-bucket-bound-error ((:value "QWE" :reason "bucket bound is not an integer/float")
                                                        "Bucket bound \"QWE\" is invalid. Reason: bucket bound is not an integer/float")))

  (subtest "Collectable Already Registered Error"
    (error-class-exists prom:collectable-already-registered-error)
    (error-report-test prom:collectable-already-registered-error ((:collectable 1 :registry 2 :rname 3)
                                                                  "Collectable 1 already registered in registry 2 with name 3"))))

(finalize)
