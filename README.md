# Prometheus.io Client

```lisp
(in-package :cl-user)

(defparameter *http-requests-counter*
  (prom:make-counter :name "http_requests" :help "Counts http request by type" :labels '("request_type")))

(prom:counter.inc *http-requests-counter* :labels '("get"))

(let ((post-requests-counter (prom:get-metric *http-requests-counter* '("post"))))
  (prom:counter.inc post-requests-counter :value 25))

(prometheus.text:marshal)

"# TYPE http_requests counter
# HELP http_requests Counts http request by type
http_requests{request_type=\"post\"} 25
http_requests{request_type=\"get\"} 1
"
```
