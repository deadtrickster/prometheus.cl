(in-package #:prometheus)

(defparameter +double-positive-infinity+
  #+sbcl sb-ext:double-float-positive-infinity
  #+ccl ccl::double-float-positive-infinity
  #+(and ecl ieee-floating-point) ext:double-float-positive-infinity
  #+allegro excl::*infinity-double*
  #+lispworks +1D++0
  #-(or sbcl ccl (and ecl ieee-floating-point) allegro lispworks)
  (error "prometheus: no double-float infinity for this implementation"))

(defparameter +double-negative-infinity+
  #+sbcl sb-ext:double-float-negative-infinity
  #+ccl ccl::double-float-negative-infinity
  #+(and ecl ieee-floating-point) ext:double-float-negative-infinity
  #+allegro excl::*negative-infinity-double*
  #+lispworks -1D++0
  #-(or sbcl ccl (and ecl ieee-floating-point) allegro lispworks)
  (error "prometheus: no double-float infinity for this implementation"))
