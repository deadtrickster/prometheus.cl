(in-package :cl-user)

(asdf:defsystem :prometheus.test.all
  :version "0.2"
  :description "All tests for Prometheus.cl"
  :maintainer "Ilya Khaprov <ilya.khaprov@publitechs.com>"
  :author "Ilya Khaprov <ilya.khaprov@publitechs.com> and CONTRIBUTORS"
  :licence "MIT"
  :depends-on ("prometheus.test"
               "prometheus.formats.text.test"
               "prometheus.exposers.hunchentoot.test"
               "prometheus.collectors.process.test"
               "prometheus.collectors.sbcl.test"
               "cl-coveralls")
  :serial t
  :components ((:file "t/all"))
  :defsystem-depends-on (:prove-asdf)
  :perform (asdf:test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
