(in-package :cl-user)

(defpackage :prometheus.test.system
  (:use :cl :asdf))

(in-package :prometheus.test.system)

(defsystem :prometheus.test
  :version "0.3"
  :description "Tests for prometheus.cl"
  :maintainer "Ilya Khaprov <ilya.khaprov@publitechs.com>"
  :author "Ilya Khaprov <ilya.khaprov@publitechs.com> and CONTRIBUTORS"
  :licence "MIT"
  :depends-on ("prometheus"
               "prometheus.test.support"
               "prove"
               "log4cl"
               "mw-equiv"
               "cl-interpol")
  :serial t
  :components ((:module "t/prometheus"
                :serial t
                :components
                ((:file "package")
                 (:test-file "dummy")
                 (:test-file "errors")
                 (:test-file "counter")
                 (:test-file "int-counter")
                 (:test-file "gauge")
                 (:test-file "summary")
                 (:test-file "histogram"))))
  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
