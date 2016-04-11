(in-package #:prometheus.hunchentoot)

(defclass exposer ()
  ((metrics-path :initform #P"metrics" :initarg :metrics-path :reader metrics-path)
   (registry :initform nil :initarg :registry :reader exposer-registry)
   (min-compress-length :initform nil :initarg :min-compress-length :reader min-compress-length)
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

(defun try-known-compression (encodings)
  (cond
    ((search "gzip" encodings)
     :gzip)
    ((search "deflate" encodings)
     :deflate)
    ((search "zlib" encodings)
     :zlib)))

(defun accept-compressed-p (request)
  (let ((accept-encodings (tbnl:header-in :accept-encoding request)))
    (if accept-encodings
        (try-known-compression accept-encodings)
        nil)))

(defmethod maybe-compress ((content string) min-compress-length request)
  (maybe-compress (trivial-utf-8:string-to-utf-8-bytes content) min-compress-length request))

(defmethod maybe-compress :around ((content vector) min-compress-length request)
  (if (and min-compress-length (>= (length content) min-compress-length))
      (call-next-method)
      content))

(defmethod compress-data ((compressor (eql :gzip)) bytes)
  (salza2:compress-data bytes
                        'salza2:gzip-compressor))

(defmethod compress-data ((compressor (eql :deflate)) bytes)
  (salza2:compress-data bytes
                        'salza2:deflate-compressor))

(defmethod compress-data ((compressor (eql :zlib)) bytes)
  (salza2:compress-data bytes
                        'salza2:zlib-compressor))

(defmethod maybe-compress ((content vector) min-compress-length request)
  (if-let ((compressor (accept-compressed-p request)))
       (progn
         (setf (tbnl:header-out :content-encoding) compressor)
         (compress-data compressor content))
       content))

(defun generate-response (acceptor request)
  (if (equal (metrics-path acceptor) (tbnl:request-pathname request))
      (progn
        (setf (tbnl:header-out "Content-Type") prom.text:+content-type+)
        (prom.text:marshal (or (exposer-registry acceptor) prom:*default-registry*)))
      "<html>
<head><title>Welcome to Prometheus Exporter!</title></head>
<body>
<h1>Prometheus Hunchentoot Exporter</h1>
<p><a href='/metrics'>Metrics</a></p>
</body>
</html>"))

(defmethod tbnl:acceptor-dispatch-request ((acceptor exposer) request)
  (maybe-compress (generate-response acceptor request)
                  (min-compress-length acceptor)
                  request))
