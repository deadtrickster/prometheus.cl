(in-package #:prometheus.formats.text.test)

(plan 1)

(subtest "Basic test"
  (with-fresh-registry
    (let ((rc (prom:make-counter :name "requests_counter" :help "Hunch\\entoot
requests counter" :labels '("type")))
          (ic (prom:make-int-counter :name "irequests_counter" :help "Hunchentoot requests counter" :labels '("type")))
          (tmg (prom:make-gauge :name "total_memory" :help "SBCL total memory"))
          (h (prom:make-histogram :name "render_time" :help "qwe" :labels '("type") :buckets '(2 4 6)))
          (ss (prom:make-simple-summary :name "traffic_summary" :help "traffic summary" :value 12))
          (s (prom:make-summary :name "qwe" :help "qwe" :value 3)))
      (prom:counter.inc rc :value 5 :labels '("ge\"t"))
      (prom:counter.inc rc :value 12 :labels '("p\\os
t"))
      (prom:counter.inc ic :value 5 :labels '("get"))
      (prom:counter.inc ic :value 12 :labels '("post"))
      (prom:gauge.set tmg 566)
      (prom:histogram.observe h 4.5 :labels '("html"))
      (prom:histogram.observe h 1 :labels '("html"))
      (prom:histogram.observe h 0.5 :labels '("html"))
      (prom:histogram.observe h 4.5 :labels '("pdf"))
      (prom:summary.observe ss 43.3d0)
      (prom:summary.observe s 5.2d0)
      (prom:summary.observe s 13)
      (prom:summary.observe s 4)
      (is (prom.text:marshal) "# TYPE requests_counter counter
# HELP requests_counter Hunch\\\\entoot\\nrequests counter
requests_counter{type=\"p\\\\os\\nt\"} 12
requests_counter{type=\"ge\\\"t\"} 5
# TYPE irequests_counter counter
# HELP irequests_counter Hunchentoot requests counter
irequests_counter{type=\"post\"} 12
irequests_counter{type=\"get\"} 5
# TYPE total_memory gauge
# HELP total_memory SBCL total memory
total_memory 566
# TYPE render_time histogram
# HELP render_time qwe
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
# TYPE qwe summary
# HELP qwe qwe
qwe{quantile=\"0.5\"} 4
qwe{quantile=\"0.9\"} 5.2
qwe{quantile=\"0.99\"} 5.2
qwe_sum 25.2
qwe_count 4
")

      (is prom.text:+content-type+ "text/plain; version=0.0.4"))))

(finalize)
