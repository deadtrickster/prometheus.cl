(in-package :cl-user)

(defparameter *http-requests-counter*
  (prom:make-counter :name "http_requests_total" :help "Counts http request by type" :labels '("method")))

(prom:counter.inc *http-requests-counter* :labels '("get"))

(let ((post-requests-counter (prom:get-metric *http-requests-counter* '("post"))))
  (prom:counter.inc post-requests-counter :value 25))

(prometheus.text:marshal)

"# TYPE http_requests counter
# HELP http_requests_total Counts http request by method
http_requests_total{request_type=\"post\"} 25
http_requests_total{request_type=\"get\"} 1
"
