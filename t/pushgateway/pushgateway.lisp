(in-package #:prometheus.pushgateway.test)

(plan 1)

(defun test-replace (address acceptor)
  (subtest "REPLACE"
    (prom.pushgateway:replace "test" :gateway address
                                     :registry (assemble-test-metrics)
                                     :grouping-key '("label" "test"))
    (let ((req (pop (my-acceptor-requests acceptor))))
      (is (first req) :put)
      (is (second req) "/metrics/job/test/label/test")
      (is (third req) prom.text:+content-type+)
      (is (fourth req) +expected-metrics-text+))))

(defun test-push (address acceptor)
  (subtest "PUSH"
    (prom.pushgateway:push "test" :gateway address
                                  :registry (assemble-test-metrics)
                                  :grouping-key '("label" "test"))
    (let ((req (pop (my-acceptor-requests acceptor))))
      (is (first req) :post)
      (is (second req) "/metrics/job/test/label/test")
      (is (third req) prom.text:+content-type+)
      (is (fourth req) +expected-metrics-text+))))

(defun test-delete (address acceptor)
  (subtest "DELETE"
    (prom.pushgateway:delete "test" :gateway address
                                    :grouping-key '("label" "test"))
    (let ((req (pop (my-acceptor-requests acceptor))))
      (is (first req) :delete)
      (is (second req) "/metrics/job/test/label/test")
      (is (third req) nil)
      (is (fourth req) nil))))

(subtest "PUSHGATEWAY"
  (subtest "Errors & Validatoins"
    (is-error-report (prom.pushgateway:delete :qwe) prom:invalid-value-error "Label value :QWE is invalid. Reason: job name is not a string")
    (is-error-report (prom.pushgateway:delete "qwe/qwe") prom:invalid-value-error "Label value \"qwe/qwe\" is invalid. Reason: job name contains / or %2f")
    (is-error-report (prom.pushgateway:delete "qwe" :grouping-key :qwe) prom:invalid-labels-error "Invalid labels. Got :QWE (type: KEYWORD), expected PLIST")
    (is-error-report (prom.pushgateway:delete "qwe" :grouping-key '(1)) prom:invalid-labels-error "Invalid labels. Got (1) (type: CONS), expected PLIST")
    (is-error-report (prom.pushgateway:delete "qwe" :grouping-key '(:qwe "qwe")) prom:invalid-label-name-error "Label name :QWE is invalid. Reason: label name is not a string")
    (is-error-report (prom.pushgateway:delete "qwe" :grouping-key '("q/we" "qwe")) prom:invalid-label-name-error "Label name \"q/we\" is invalid. Reason: label name doesn't match regex [a-zA-Z_][a-zA-Z0-9_]*")
    (is-error-report (prom.pushgateway:delete "qwe" :grouping-key '("qwe" :qwe)) prom:invalid-label-value-error "Label value :QWE is invalid. Reason: label value is not a string")
    (is-error-report (prom.pushgateway:delete "qwe" :grouping-key '("qwe" "q/we")) prom:invalid-label-value-error "Label value \"q/we\" is invalid. Reason: label value contains / or %2f"))

  (subtest "API"
    (let ((metrics-acceptor)
          (pushgateway-address "localhost:9131"))
      (unwind-protect
           (progn
             (setf metrics-acceptor (tbnl:start (make-instance 'my-acceptor :address "localhost"
                                                                            :port 9131)))
             (sleep 1)
             (test-replace pushgateway-address metrics-acceptor)
             (test-push pushgateway-address metrics-acceptor)
             (test-delete pushgateway-address metrics-acceptor))
        (when metrics-acceptor
          (tbnl:stop metrics-acceptor :soft t))))))

(finalize)
