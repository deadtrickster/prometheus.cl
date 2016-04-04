(in-package #:prometheus.test)

(plan 1)

(defmacro with-fresh-registry (&body body)
  `(let ((prom:*default-registry* (make-instance 'prom:registry)))
     ,@body))

(subtest "Basic test"
  (with-fresh-registry
    (let ((rc (prom:make-counter :name "requests_counter" :help "Hunchentoot requires counter" :labels '("type")))
          (tmg (prom:make-gauge :name "total_memory" :help "SBCL total memory"))
          (h (prom:make-histogram :name "render_time" :help "" :labels '("type") :buckets '(2 4 6)))
          (s (prom:make-summary :name "traffic_summary" :help "traffic summary" :value 12)))
      (prom:counter.inc rc :value 5 :labels '("get"))
      (prom:counter.inc rc :value 12 :labels '("post"))
      (prom:gauge.set tmg 566)
      (prom:histogram.observe h 4.5 :labels '("html"))
      (prom:histogram.observe h 1 :labels '("html"))
      (prom:histogram.observe h 0.5 :labels '("html"))
      (prom:histogram.observe h 4.5 :labels '("pdf"))
      (prom:summary.observe s 43.3d0)
      (is (prometheus.text:marshal) "# TYPE requests_counter counter
# HELP requests_counter Hunchentoot requires counter
requests_counter{type=\"post\"} 12
requests_counter{type=\"get\"} 5
# TYPE total_memory gauge
# HELP total_memory SBCL total memory
total_memory 566
# TYPE render_time histogram
# HELP render_time 
render_time_bucket{type=\"pdf\", le=\"2\"} 0
render_time_bucket{type=\"pdf\", le=\"4\"} 0
render_time_bucket{type=\"pdf\", le=\"6\"} 1
render_time_bucket{type=\"pdf\", le=\"+Inf\"} 1
render_time_sum{type=\"pdf\"} 4.5
render_time_count{type=\"pdf\"} 1
render_time_bucket{type=\"html\", le=\"2\"} 2
render_time_bucket{type=\"html\", le=\"4\"} 2
render_time_bucket{type=\"html\", le=\"6\"} 3
render_time_bucket{type=\"html\", le=\"+Inf\"} 3
render_time_sum{type=\"html\"} 6.0
render_time_count{type=\"html\"} 3
# TYPE traffic_summary summary
# HELP traffic_summary traffic summary
traffic_summary_sum 55.3
traffic_summary_count 2
"))))

(finalize)
