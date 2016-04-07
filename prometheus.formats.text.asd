(asdf:defsystem :prometheus.formats.text
  :serial t
  :version "0.0.1"
  :licence "MIT"
  :depends-on ("prometheus"
               "alexandria")
  :author "Ilya Khaprov <ilya.kharpov@publitechs.com>"
  :components ((:module "src/formats/text"
                :serial t
                :components
                ((:file "package")
                 (:file "text"))))
  :description "Prometheus.io Client Text format")
