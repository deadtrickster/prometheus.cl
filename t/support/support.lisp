(in-package #:prometheus.test.support)

(defmacro error-class-exists (name &optional (base 'prom:base-error))
  (let ((class-exists-msg (format nil "~s class exists" name))
        (class-is-base-msg (format nil "~s is ~s" name base))
        (no-class-msg (format nil "No ~s class" name)))
    `(subtest "Error existence check"
       (let ((error-class (find-class ',name nil)))
         (if error-class
             (progn
               (pass ,class-exists-msg)
               (is (subtypep error-class ',base) t ,class-is-base-msg))
             (fail ,no-class-msg))))))

(defmacro is-error-report (form error report)
  `(is
    (handler-case
        ,form
      (,error (e) (princ-to-string e)))
    ,report
    ,(format nil "~s throwed ~a reported as ~s"
             form
             error
             report)))

(defmacro error-report-test (class &rest tests)
  `(subtest "Error report test(s)"
     ,@(loop for (args result) in tests
             collect `(is
                       (handler-case
                           (error ',class ,@args)
                         (,class (e) (princ-to-string e)))
                       ,result
                       ,(format nil "~s reported as ~s"
                                `(error ',class ,@args)
                                result)))))

(defmacro with-fresh-registry (&body body)
  `(let ((prom:*default-registry* (prom:make-registry)))
     ,@body))
