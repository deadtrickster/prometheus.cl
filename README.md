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

## Metric Types

- Counter
- Gauge
- Histogram

## Custom collectors

### SBCL runtime information
 - Threads
 - Memory

## Exposers

### Hunchentoot

## Example

Hunchentoot exposer plus SBCL metrics.

```lisp
(prom.sbcl:make-memory-collector)
(prom.sbcl:make-threads-collector)
(defclass my-acceptor (prom.tbnl::hunchentoot-exposer tbnl:acceptor)
  ())
(tbnl:start (make-instance 'my-acceptor :address "172.17.0.1" :port 9101))
```
will produce something like this:

![SBCL Dashboard](http://i.imgur.com/5FarndD.png)

Effect of `(sb-ext:gc)` can be seen clearly.

## License
MIT
