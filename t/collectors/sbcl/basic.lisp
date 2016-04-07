(in-package #:prometheus.collectors.sbcl.test)

(plan 1)

(subtest "Sbcl Collectors test"
  (subtest "Threads Collector"
    (with-fresh-registry
      (let ((pc (prom.sbcl:make-threads-collector :namespace "qwe_"))
            (metrics)
            (expected-names '("qwe_sbcl_threads")))
        (is (prom:registeredp "sbcl_threads_collector") pc "Collector registers itself")
        (prom:collect pc (lambda (m)
                           (push m metrics)))
        (is (length metrics) 1 "Should export 1 metric")
        (subtest "Checking exported metrics"
          (loop for metric in (reverse metrics)
                for ename in expected-names do
                   (is (prom:metric-family-name metric) ename))))))

  (subtest "Memory Collector"
    (with-fresh-registry
      (let ((pc (prom.sbcl:make-memory-collector :namespace "qwe_"))
            (metrics)
            (expected-names '("qwe_sbcl_read_only_bytes"
                              "qwe_sbcl_static_bytes"
                              "qwe_sbcl_dynamic_bytes"
                              "qwe_sbcl_dynamic_objects")))
        (is (prom:registeredp "sbcl_memory_collector") pc "Collector registers itself")
        (prom:collect pc (lambda (m)
                           (push m metrics)))
        (is (length metrics) 4 "Should export 4 metrics")
        (subtest "Checking exported metrics"
          (loop for metric in (reverse metrics)
                for ename in expected-names do
                   (is (prom:metric-family-name metric) ename)))))))

(finalize)
