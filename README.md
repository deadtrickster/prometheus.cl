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
- Summary (with quantiles, CKMS via quantile-estimator)
- Quantile Summary (DDSketch: relative-error quantiles, mergeable across series, sharded per thread and lock-free on SBCL; port of prometheus.erl's `prometheus_quantile_summary`)

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

## Performance

Hot paths allocate nothing and use no CLOS. Resolve a series once and keep it - this skips the label lookup:

```lisp
(defvar *requests* (prom:get-metric requests-counter '("get")))
(prom:counter.inc *requests*)
```

Counters and quantile summaries are sharded per CPU, so threads on different CPUs do not contend. Label lookups are lock-free.

SBCL 2.6.10, 4 CPUs, 32 threads, million ops/s:

| | 0.4 | now |
|---|---|---|
| `counter.inc` on a series | 4.2 | 55.6 |
| `counter.inc` with labels | 1.6 | 26.3 |
| `gauge.set` on a series, 1 thread, ns/op | 59.6 | 8.0 |
| summary observe (CKMS -> DDSketch) | 0.3 | 19.2 |

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
