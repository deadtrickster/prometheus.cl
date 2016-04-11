(in-package #:prometheus.exposers.hunchentoot.test)

(plan 1)

(defclass my-acceptor (prom.tbnl:exposer tbnl:acceptor)
  ())

(defun gunzip (vector)
  (trivial-utf-8:utf-8-bytes-to-string (chipz:decompress nil 'chipz:gzip vector)))

(defun test-metrics-endpoint (expected-metrics-text)
  (subtest "Metrics"
    (multiple-value-bind (body status-code headers)
        (drakma:http-request "http://localhost:9101/metrics" :force-binary t :additional-headers '(("accept-encoding" . "gzip")))
      (is status-code 200)
      (is (drakma:header-value :content-encoding headers) "GZIP")
      (is (gunzip body) expected-metrics-text "Metrics endpoint sent expected text")
      (ok (starts-with-subseq prom.text:+content-type+ (drakma:header-value :content-type headers)) "Right content-type"))))

(defun test-greeting (expected-greeting-text)
  (subtest "Greeting"
    (multiple-value-bind (body status-code headers)
        (drakma:http-request "http://localhost:9101/" :additional-headers '(("accept-encoding" . "gzip")))
      (is status-code 200)
      (is body expected-greeting-text "Greeting sent expected text")
      (ok (starts-with-subseq "text/html" (drakma:header-value :content-type headers)) "Right content-type"))))

(subtest "Hunchentoot Exposer test"
  (with-fresh-registry
    (let ((rc (prom:make-counter :name "requests_counter" :help "Hunchentoot requires counter" :labels '("type")))
          (tmg (prom:make-gauge :name "total_memory" :help "SBCL total memory"))
          (h (prom:make-histogram :name "render_time" :help "" :labels '("type") :buckets '(2 4 6)))
          (s (prom:make-summary :name "traffic_summary" :help "traffic summary" :value 12))
          (expected-metrics-text "# TYPE requests_counter counter
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
")
          (expected-greeting-text "<html>
<head><title>Welcome to Prometheus Exporter!</title></head>
<body>
<h1>Prometheus Hunchentoot Exporter</h1>
<p><a href='/metrics'>Metrics</a></p>
</body>
</html>"))
      (prom:counter.inc rc :value 5 :labels '("get"))
      (prom:counter.inc rc :value 12 :labels '("post"))
      (prom:gauge.set tmg 566)
      (prom:histogram.observe h 4.5 :labels '("html"))
      (prom:histogram.observe h 1 :labels '("html"))
      (prom:histogram.observe h 0.5 :labels '("html"))
      (prom:histogram.observe h 4.5 :labels '("pdf"))
      (prom:summary.observe s 43.3d0)
      
      (let ((metrics-acceptor))
        (unwind-protect
             (progn
               (setf metrics-acceptor (tbnl:start (make-instance 'my-acceptor :registry prom:*default-registry*
                                                                              :address "localhost"
                                                                              :port 9101
                                                                              :min-compress-length 512)))
               (test-metrics-endpoint expected-metrics-text)
               (test-greeting expected-greeting-text))
          (when metrics-acceptor
            (tbnl:stop metrics-acceptor :soft t)))))))

(finalize)
