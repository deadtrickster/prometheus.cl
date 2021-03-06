(asdf:defsystem :prometheus
  :serial t
  :version "0.4.1"
  :licence "MIT"
  :depends-on ("alexandria"
               "bordeaux-threads"
               "cl-ppcre"
               "local-time"
               "quantile-estimator")
  :author "Ilya Khaprov <ilya.kharpov@publitechs.com>"
  :components ((:module "src/prometheus"
                :serial t
                :components
                ((:file "package")
                 (:module "base"
                  :serial t
                  :components
                  ((:file "errors")
                   (:file "cas")
                   (:file "synchronizable")
                   (:file "collectable")
                   (:file "metrics-storage")
                   (:file "timing")))
                 (:file "labels")
                 (:file "metric")
                 (:file "collector")
                 (:file "registry")
                 (:file "buckets")
                 (:module "metrics"
                  :serial t
                  :components
                  ((:file "counter")
                   (:file "gauge")
                   (:file "histogram")
                   (:file "summary")
                   (:file "int-counter"))))))
  :description "Prometheus.io Client")
