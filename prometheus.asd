(asdf:defsystem :prometheus
  :serial t
  :version "0.0.1"
  :licence "MIT"
  :depends-on ("alexandria"
               "bordeaux-threads"
               "cl-ppcre"
               "local-time")
  :author "Ilya Khaprov <ilya.kharpov@publitechs.com>"
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:module "base"
                  :serial t
                  :components
                  ((:file "synchronizable")
                   (:file "collectable")
                   (:file "metrics-storage")
                   (:file "timing")))
                 (:file "labels")
                 (:file "metric")
                 (:file "collector")
                 (:file "registry")
                 (:module "metrics"
                  :serial t
                  :components
                  ((:file "counter")
                   (:file "gauge")
                   (:file "histogram")
                   (:file "summary")))
                 (:module "formats"
                  :serial t
                  :components
                  ((:file "text"))))))
  :description "Prometheus.io Client")
