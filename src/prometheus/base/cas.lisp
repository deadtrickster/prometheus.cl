(in-package :prometheus)

(defmacro cas (place old new &environment env)
  (declare (ignorable env))
  (check-type old symbol)
  ;; macroexpand is needed for sbcl-1.0.53 and older
  #+sbcl `(eql ,old (sb-ext:compare-and-swap ,place
                                            ,old ,new))
  #+lispworks `(sys:compare-and-swap ,place ,old ,new))

(defmacro cas-incf (place inc)
  `(loop
     as old = ,place
     when (cas ,place old (incf old ,inc)) do
     (return)))
