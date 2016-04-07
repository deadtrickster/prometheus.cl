(in-package :prometheus.test)

(plan 1)

(defmacro is-histogram-stat (histogram &rest stat)
  `(let ((buckets (prom:histogram-buckets ,histogram)))
     (assert (= (length ',stat) (length buckets)))
     (let ((i -1))
       (dolist (count ',stat)
         (let ((bucket (aref buckets (incf i))))
           (is (prom::bucket-count bucket) count (format nil "Bucket [~a] is expected to have ~a elements" (prom::bucket-bound bucket) count)))))))

(subtest "Histogram"
  (subtest "Errors & Validations"
    (is-error-report (prom:histogram.observe (prom:make-histogram :name "qwe" :buckets '(1) :registry nil) "qwe") prom:invalid-value-error "Value \"qwe\" is invalid. Reason: value is not a number")
    (is-error-report (prom:make-histogram :name "qwe" :buckets '(1) :value "qwe" :registry nil) prom:invalid-value-error "Value \"qwe\" is invalid. Reason: value is not a number")
    (is-error-report (prom:make-histogram :name "qwe" :buckets '(1) :value '(1 "qwe") :registry nil) prom:invalid-value-error "Value \"qwe\" is invalid. Reason: value is not a number")
    (is-error-report (prom:make-histogram :name "qwe" :value 12 :labels '("qwe") :registry nil) prom:invalid-value-error "Value 12 is invalid. Reason: can only specify at most one of value and labels")
    (is-error-report (prom:make-histogram :name "qwe" :buckets "qwe" :registry nil) prom:invalid-buckets-error "Invalid buckets. Got \"qwe\" (type: (SIMPLE-ARRAY CHARACTER (3))), reason: expected LIST")
    (is-error-report (prom:make-histogram :name "qwe" :buckets '() :registry nil) prom:invalid-value-error "Invalid buckets. Got NIL (type: NULL), reason: must be at least one bucket")
    (is-error-report (prom:make-histogram :name "qwe" :buckets '("qwe") :registry nil) prom:invalid-value-error "Bucket bound \"qwe\" is invalid. Reason: bucket bound is not an integer/float")
    (is-error-report (prom:make-histogram :name "qwe" :buckets '(1 3 2) :registry nil) prom:invalid-value-error "Invalid buckets. Got (1 3 2) (type: CONS), reason: bounds not sorted"))

  (subtest "OBSERVE"
    (let* ((h (prom:make-histogram :name "qwe" :buckets '(2 4 6) :value '(4.5 1 0.5) :registry nil))
           (nlm (prom:get-metric h nil)))
      (is (prom:histogram-sum nlm) 6.0)
      (is (prom:histogram-count nlm) 3)
      (is-histogram-stat nlm 2 0 1 0)
      (prom:histogram.observe h 4)
      (prom:histogram.observe h 12)
      (is-histogram-stat nlm 2 1 1 1)))

  (subtest "TIME"
    (let* ((c (prom:make-histogram :name "qwe" :buckets '(1 2) :registry nil))
           (nlm (prom:get-metric c nil)))
      (prom:histogram.time nlm (sleep 0.5))
      (ok (and (>= (prom:histogram-sum nlm) 500)
               (< (prom:histogram-sum nlm) 510)))
      (is (prom:histogram-count nlm) 1)))

  (subtest "REGISTRY"
    (with-fresh-registry
      (let ((s (prom:make-histogram :name "qwe" :buckets '(1 2))))
        (is (prom:registeredp s prom:*default-registry*) s)))))

(finalize)
