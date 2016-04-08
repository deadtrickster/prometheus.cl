(asdf:defsystem :prometheus.examples
  :serial t
  :version "0.1"
  :licence "MIT"
  :depends-on ("prometheus"
               "prometheus.formats.text"
               "prometheus.exposers.hunchentoot"
               "prometheus.collectors.process"
               "prometheus.collectors.sbcl")
  :author "Ilya Khaprov <ilya.kharpov@publitechs.com>"
  :components ((:module "examples"
                :serial t
                :components
                ((:file "package")
                 (:file "http"))))
  :description "Prometheus.io Client Example")
