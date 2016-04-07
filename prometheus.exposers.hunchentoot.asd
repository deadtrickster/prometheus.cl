(asdf:defsystem :prometheus.exposers.hunchentoot
  :serial t
  :version "0.0.1"
  :licence "MIT"
  :depends-on ("prometheus" "prometheus.formats.text" "hunchentoot")
  :author "Ilya Khaprov <ilya.kharpov@publitechs.com>"
  :components ((:module "src/exposers/hunchentoot"
                :serial t
                :components
                ((:file "package")
                 (:file "hunchentoot"))))
  :description "Expose Prometheus.io client metrics using Hunchentoot")
