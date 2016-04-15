(in-package #:prometheus.pushgateway.test)

(defclass my-acceptor (tbnl:acceptor)
  ((requests :initform nil :accessor my-acceptor-requests)))

(defmethod tbnl:acceptor-log-access ((acceptor my-acceptor) &key return-code)
  (declare (ignore return-code)))

(defmethod tbnl:acceptor-log-message ((acceptor my-acceptor) log-level format-string &rest format-arguments)
  (declare (ignore log-level format-string format-arguments)))

(defmethod tbnl:acceptor-dispatch-request ((acceptor my-acceptor) request)
  (if (search "error" (tbnl:request-uri request))
      (progn
        (setf (tbnl:return-code*) tbnl:+http-not-found+)        
        "Not Found!")
      (progn
        (push (list (tbnl:request-method request)
                    (tbnl:request-uri request)
                    (tbnl:header-in :content-type request)
                    (tbnl:raw-post-data :request request :force-text t))
              (my-acceptor-requests acceptor))
        (setf (tbnl:return-code*) tbnl:+http-accepted+)
        (tbnl:abort-request-handler))))

(defmethod tbnl::acceptor-status-message ((acceptor my-acceptor) http-status-code &rest args &key &allow-other-keys)
  (case http-status-code
    (404
     "Not Found!")
    (t (apply 'tbnl::make-cooked-message http-status-code args))))
