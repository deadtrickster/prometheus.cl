(cl:eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op 'cffi-grovel))

(asdf:defsystem :prometheus.collectors.process
  :serial t
  :version "0.0.1"
  :licence "MIT"
  :depends-on ("prometheus"
               "cl-fad"
               "split-sequence"
               "cffi")
  :author "Ilya Khaprov <ilya.kharpov@publitechs.com>"
  :components ((:module "src/collectors/process"
                :serial t
                :components
                ((:file "package")
                 (cffi-grovel:grovel-file "grovel")
                 (:file "ffi")
                 (:file "process"))))
  :description "Prometheus.io Process Info Collector")
