(asdf:defsystem :prometheus.hunchentoot
  :serial t
  :version "0.0.1"
  :licence "MIT"
  :depends-on ("prometheus" "hunchentoot")
  :author "Ilya Khaprov <ilya.kharpov@publitechs.com>"
  :components ((:module "exposers/http"
                :serial t
                :components
                ((:module "hunchentoot"
                   :serial t
                   :components
                   ((:file "package")
                    (:file "hunchentoot"))))))
  :description "Expose Prometheus.io client metrics using Hunchentoot")
