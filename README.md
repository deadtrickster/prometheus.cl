# Prometheus.io Common Lisp Client [![Build Status](https://travis-ci.org/deadtrickster/prometheus.cl.svg?branch=master)](https://travis-ci.org/deadtrickster/prometheus.cl) [![Coverage Status](https://coveralls.io/repos/github/deadtrickster/prometheus.cl/badge.svg?branch=master)](https://coveralls.io/github/deadtrickster/prometheus.cl?branch=master)

Example Grafana dashboard for Hunchentoot on SBCL:

![Prometheus + Grafan + SBCL + Hunchentoot](http://i.imgur.com/oO2murq.png)

You can get this dashboard [here](https://raw.githubusercontent.com/deadtrickster/prometheus.cl/master/dashboards/HunchentootSBCL.json).

### Example Quick Start
Currently example uses Linux and SBCL specific collectors. 

```lisp
(ql:quickload :prometheus.examples)
(prometheus.example:run)
```
You can override app/exporter host/port in `prometheus.example:run` arguments. To stop example app call `prometheus.example:stop`


## Metric Types

- Counter
- Int Counter (can only work with unsigned int64)
- Gauge
- Histogram
- Simple Summary (without quantiles)
- Summary (with quantiles)

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

## Performance / Optimization

### Counter
On SBCL counter can use CAS. On SBCL int counter can use atomic-incf.

Benchmark (30 threads each doing 100000 counter.inc):

| Method        | Avg inc n/s |
| ------------- |:-----------:|
| Mutex         | 7885        |
| CAS (SBCL)    | 1902        |
| ATOMIC (SBCL) | 141         |

### Gauge
On SBCL gauge can use CAS.

Benchmark (30 threads each doing 100000 gauge.set):

| Method        | Avg set n/s |
| ------------- |:-----------:|
| Mutex         | 9618        |
| CAS (SBCL)    | 2204        |

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
