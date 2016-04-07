(in-package #:prometheus)

(defun timing% (setter lambda)
  (multiple-value-bind (sec1 nsec1) (local-time::%get-current-time)
     (multiple-value-prog1
         (funcall lambda)
       (multiple-value-bind (sec2 nsec2) (local-time::%get-current-time)
         (let ((msec-d  (floor (- nsec2 nsec1) 1000000))
               (sec-d (- sec2 sec1)))
           (when (< msec-d 0)
             (decf sec-d)
             (incf msec-d 1000))
           (funcall setter (+ (* sec-d 1000) msec-d)))))))
