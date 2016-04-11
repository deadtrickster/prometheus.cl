(in-package :cl-user)

(asdf:defsystem :prometheus.exposers.hunchentoot.test
  :version "0.1"
  :description "Tests for prometheus hunchentoot exposer"
  :maintainer "Ilya Khaprov <ilya.khaprov@publitechs.com>"
  :author "Ilya Khaprov <ilya.khaprov@publitechs.com> and CONTRIBUTORS"
  :licence "MIT"
  :depends-on ("prometheus.exposers.hunchentoot"
               "prometheus.formats.text"
               "drakma"
               "prove"
               "log4cl"
               "mw-equiv"
               "cl-interpol"
               "chipz")
  :serial t
  :components ((:module "t/exposers/hunchentoot"
                :serial t
                :components
                ((:file "package")
                 (:test-file "dummy")
                 (:test-file "basic"))))
  :defsystem-depends-on (:prove-asdf)
  :perform (asdf:test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
