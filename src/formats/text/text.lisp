(in-package #:prometheus.formats.text)

(define-constant +content-type+ "text/plain; version=0.0.4" :test #'equal)

(defmethod catch-strange-float (value)
  value)

(defmethod catch-strange-float ((value (eql #+sbcl sb-ext:double-float-positive-infinity #+(and ecl ieee-floating-point) EXT:DOUBLE-FLOAT-POSITIVE-INFINITY)))
  "+Inf")

(defmethod catch-strange-float ((value (eql #+sbcl sb-ext:double-float-negative-infinity #+(and ecl ieee-floating-point) EXT:DOUBLE-FLOAT-negative-INFINITY)))
  "-Inf")

(defun print-sample-line (stream name lables lvalues value)
  (let ((*read-default-float-format* 'double-float))
    (format stream "~a~@[{~{~{~(~A~)=\"~A\"~}~^, ~}}~] ~a~%" name (mapcar (lambda (lname lvalue)
                                                                            (list lname (if (stringp lvalue)
                                                                                            (escape-label-value lvalue)
                                                                                            lvalue))) lables lvalues) (if (floatp value) (coerce value 'double-float) value))))

(defgeneric metric-to-text (metric name family-lables stream)
  (:method ((metric prom:simple-metric) name family-lables stream)
    (let ((*read-default-float-format* 'double-float))
      (print-sample-line stream name family-lables (prom:metric-labels metric) (prom:metric-value metric))))
  (:method ((metric prom:histogram-metric) name labels stream)
    (let ((bucket-name (concatenate 'string name "_bucket"))
          (le-labels (append labels (list "le")))
          (counter 0))
      (loop for bucket across (prom:histogram-buckets metric)
            do
               (print-sample-line stream bucket-name le-labels (append (prom:metric-labels metric) (list (catch-strange-float (prom:bucket-bound bucket)))) (incf counter (prom:bucket-count bucket))))
      (print-sample-line stream
                         (concatenate 'string name "_sum")
                         labels
                         (prom:metric-labels metric)
                         (prom:histogram-sum metric))
      (print-sample-line stream
                         (concatenate 'string name "_count")
                         labels
                         (prom:metric-labels metric)
                         counter)))
  (:method ((metric prom:summary-metric) name labels stream)
    (print-sample-line stream
                       (concatenate 'string name "_sum")
                       labels
                       (prom:metric-labels metric)
                       (prom:summary-sum metric))
    (print-sample-line stream
                       (concatenate 'string name "_count")
                       labels
                       (prom:metric-labels metric)
                       (prom:metric-value metric))))

(defgeneric metric-family-to-text (mf stream)
  (:method ((mf prom::metric-family) stream)
    (let ((name (prom::metric-family-name mf))
          (labels (prom:metric-family-labels mf)))
      (format stream "# TYPE ~a ~a~%" name (prom:metric-family-type mf))
      (format stream "# HELP ~a ~a~%" name (escape-metric-help (prom:metric-family-help mf)))
      (loop for metric in (prom::get-metrics mf) do
            (metric-to-text metric name labels stream)))))

(defgeneric marshal% (registry)
  (:method ((registry prom:registry))
    (with-output-to-string (stream)
      (prom::collect registry
        (lambda (mf)
          (metric-family-to-text mf stream))))))

(defun marshal (&optional (registry prom:*default-registry*))
  (marshal% registry))
