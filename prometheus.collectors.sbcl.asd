(asdf:defsystem :prometheus.collectors.sbcl
  :serial t
  :version "0.1"
  :licence "MIT"
  :depends-on ("prometheus")
  :author "Ilya Khaprov <ilya.kharpov@publitechs.com>"
  :components ((:module "src/collectors/sbcl"
                :serial t
                :components
                ((:file "package")
                 (:file "threads")
                 (:file "memory"))))
  :description "Prometheus.io SBCL Collector")
