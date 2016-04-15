(in-package :cl-user)

(asdf:defsystem :prometheus.formats.text.test
  :version "0.3"
  :description "Tests for prometheus text format"
  :maintainer "Ilya Khaprov <ilya.khaprov@publitechs.com>"
  :author "Ilya Khaprov <ilya.khaprov@publitechs.com> and CONTRIBUTORS"
  :licence "MIT"
  :depends-on ("prometheus.test.support"
               "prometheus.formats.text"
               "prove"
               "log4cl"
               "mw-equiv"
               "cl-interpol")
  :serial t
  :components ((:module "t/formats/text"
                :serial t
                :components
                ((:file "package")
                 (:test-file "dummy")
                 (:test-file "basic"))))
  :defsystem-depends-on (:prove-asdf)
  :perform (asdf:test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
