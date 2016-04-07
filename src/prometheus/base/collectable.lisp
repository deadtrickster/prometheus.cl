(in-package #:prometheus)

(defclass collectable ()
  ((name :initarg :name :reader collectable-name)))

(defgeneric collect (collectable cb))
