(asdf:defsystem :prometheus.formats.text
  :serial t
  :version "0.2"
  :licence "MIT"
  :depends-on ("prometheus"
               "alexandria")
  :author "Ilya Khaprov <ilya.kharpov@publitechs.com>"
  :components ((:module "src/formats/text"
                :serial t
                :components
                ((:file "package")
                 (:file "escape")
                 (:file "text"))))
  :description "Prometheus.io Client Text format")
