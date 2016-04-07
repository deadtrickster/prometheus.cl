(in-package #:prometheus)

(defclass collector (collectable)
  ((namespace :initarg :namespace :initform "" :reader collector-namespace)))

(defmethod collector-metric-name ((collector collector) name)
  (concatenate 'string
               (collector-namespace collector)
               name))
