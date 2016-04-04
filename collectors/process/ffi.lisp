(in-package #:prometheus.process)

(cffi:defcfun sysconf :long
  (name :int))
