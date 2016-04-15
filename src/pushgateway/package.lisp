(in-package #:cl-user)

(defpackage #:prometheus.pushgateway
  (:use #:cl #:alexandria)
  (:nicknames #:prom.pushgateway)
  (:shadow #:push
           #:replace
           #:delete)
  (:export #:push
           #:replace
           #:delete

           #:pushgateway-error))
