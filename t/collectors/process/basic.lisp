(in-package #:prometheus.collectors.process.test)

(plan 1)

(subtest "Process Collector test"
  (with-fresh-registry
    (let ((pc (prom.process:make-process-collector :namespace "qwe_"))
          (metrics)
          (expected-names '("qwe_process_open_fds"
                            "qwe_process_max_fds"
                            "qwe_process_start_time_seconds"
                            "qwe_process_uptime_seconds"
                            "qwe_process_threads_total"
                            "qwe_process_virtual_memory_bytes"
                            "qwe_process_resident_memory_bytes"
                            "qwe_process_cpu_seconds"
                            "qwe_process_cpu_seconds_total")))
      (is (prom:registeredp "process_collector") pc "Collector registers itself")
      (prom:collect pc (lambda (m)
                         (push m metrics)))
      (is (length metrics) 9 "Should export 9 metrics")
      (subtest "Checking exported metrics"
        (loop for metric in (reverse metrics)
              for ename in expected-names do
                 (is (prom:metric-family-name metric) ename))))))

(finalize)
