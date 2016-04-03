(in-package #:prometheus)

(defclass synchronizable ()
  ((lock :initform (bt:make-lock) :reader synchronizable-lock)))

(defmacro synchronize (synchronizable &body body)
  `(bt:with-lock-held ((synchronizable-lock ,synchronizable))
     ,@body))
