(in-package :cl-user)

(asdf:defsystem :prometheus.collectors.sbcl.test
  :version "0.1"
  :description "Tests for prometheus SBCL collector"
  :maintainer "Ilya Khaprov <ilya.khaprov@publitechs.com>"
  :author "Ilya Khaprov <ilya.khaprov@publitechs.com> and CONTRIBUTORS"
  :licence "MIT"
  :depends-on ("prometheus.collectors.sbcl"
               "prove"
               "log4cl"
               "mw-equiv"
               "cl-interpol")
  :serial t
  :components ((:module "t/collectors/sbcl"
                :serial t
                :components
                ((:file "package")
                 (:test-file "dummy")
                 (:test-file "basic"))))
  :defsystem-depends-on (:prove-asdf)
  :perform (asdf:test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
