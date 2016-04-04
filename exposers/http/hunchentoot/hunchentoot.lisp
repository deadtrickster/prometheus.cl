(in-package #:prometheus.hunchentoot)

(defclass exposer ()
  ((metrics-path :initform #P"metrics" :initarg :metrics-path :reader metrics-path)
   (mute-access-logs :initform t :initarg :mute-access-logs :reader mute-access-logs)
   (mute-messages-logs :initform t :initarg :mute-error-logs :reader mute-messages-logs)))

(defmethod tbnl:acceptor-log-access ((acceptor exposer) &key return-code)
  (declare (ignore return-code))
  (unless (mute-access-logs acceptor)
    (call-next-method)))

(defmethod tbnl:acceptor-log-message ((acceptor exposer) log-level format-string &rest format-arguments)
  (declare (ignore log-level format-string format-arguments))
  (unless (mute-messages-logs acceptor)
    (call-next-method)))

(defmethod tbnl:acceptor-dispatch-request ((acceptor exposer) request)
  (if (equal (metrics-path acceptor) (tbnl:request-pathname request))
      (progn
        (setf (tbnl:header-out "Content-Type") prom.text:+content-type+)
        (prom.text:marshal))
      "<html>
<head><title>Welcome to Prometheus Exporter!</title></head>
<body>
<h1>Node Exporter</h1>
<p><a href='/metrics'>Metrics</a></p>
</body>
</html>"))
