(in-package :cl-user)

(asdf:defsystem :prometheus.test.support
  :version "0.1"
  :description "Support stuff for prometheus client tests"
  :maintainer "Ilya Khaprov <ilya.khaprov@publitechs.com>"
  :author "Ilya Khaprov <ilya.khaprov@publitechs.com> and CONTRIBUTORS"
  :licence "MIT"
  :depends-on ("alexandria"
               "prometheus"
               "prove")
  :serial t
  :components ((:module "t/support"
                :serial t
                :components
                ((:file "package")
                 (:file "support"))))
  :defsystem-depends-on (:prove-asdf)
  :perform (asdf:test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
