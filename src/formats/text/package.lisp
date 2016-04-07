(in-package #:cl-user)

(defpackage #:prometheus.formats.text
  (:use #:cl #:alexandria)
  (:nicknames #:prom.text)
  (:export #:+content-type+
           #:marshal))
