(asdf:defsystem :prometheus.pushgateway
  :serial t
  :version "0.1"
  :licence "MIT"
  :depends-on ("prometheus"
               "prometheus.formats.text"
               "drakma")
  :author "Ilya Khaprov <ilya.kharpov@publitechs.com>"
  :components ((:module "src/pushgateway"
                :serial t
                :components
                ((:file "package")
                 (:file "pushgateway"))))
  :description "Prometheus.io Pushgateway client")
