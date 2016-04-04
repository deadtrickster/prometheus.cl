(asdf:defsystem :prometheus.sbcl
  :serial t
  :version "0.0.1"
  :licence "MIT"
  :depends-on ("prometheus")
  :author "Ilya Khaprov <ilya.kharpov@publitechs.com>"
  :components ((:module "collectors/sbcl"
                :serial t
                :components
                ((:file "package")
                 (:file "threads")
                 (:file "memory"))))
  :description "Prometheus.io SBCL Collector")
