# Prometheus.io Common Lisp Client

Example Grafana dashboard for Hunchentoot on SBCL:

![Prometheus + Grafan + SBCL + Hunchentoot](http://i.imgur.com/8CBhHPU.png)

## Metric Types

- Counter
- Gauge
- Histogram
- Summary (without quantiles for now)

## Custom collectors

### SBCL runtime information
 - Threads
 - Memory
 
### Process information
 - Open fds count
 - Max fds count
 - Virtual memory bytes
 - Resident memory bytes
 - Process CPU seconds{stime|utime} (total)
 - Process start time (Unix epoch)
 - Process uptime
 
Linux? only

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
