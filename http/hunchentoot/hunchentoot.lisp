(in-package #:prometheus.hunchentoot)

(defclass hunchentoot-exposer ()
  ((metrics-path :initform #P"metrics" :initarg :metrics-path :reader metrics-path)))

(defmethod tbnl:acceptor-dispatch-request ((acceptor hunchentoot-exposer) request)
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
