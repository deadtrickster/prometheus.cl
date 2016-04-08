(in-package #:prometheus.example)

(defparameter *example-registry* nil)

(defparameter *http-requests-counter* nil)

(defparameter *http-request-duration* nil)

(defclass exposer-acceptor (prom.tbnl:exposer tbnl:acceptor)
  ())

(defclass application (tbnl:acceptor)
  ((exposer :initarg :exposer :reader application-metrics-exposer)
   (mute-access-logs :initform t :initarg :mute-access-logs :reader mute-access-logs)
   (mute-messages-logs :initform t :initarg :mute-error-logs :reader mute-messages-logs)))

(defun initialize-metrics ()
  (unless *example-registry*
    (setf *example-registry* (prom:make-registry))
    (let ((prom:*default-registry* *example-registry*))
      (setf *http-requests-counter*
            (prom:make-counter :name "http_requests_total"
                               :help "Counts http request by type"
                               :labels '("method" "app")))
      (setf *http-request-duration* (prom:make-histogram :name "http_request_duration_milliseconds"
                                                         :help "HTTP requests duration[ms]"
                                                         :labels '("method" "app")
                                                         :buckets '(10 25 50 75 100 250 500 750 1000 1500 2000 3000)))
      #+sbcl
      (prom.sbcl:make-memory-collector)
      #+sbcl
      (prom.sbcl:make-threads-collector)
      (prom.process:make-process-collector))))

(defun run (&key (exposer-port 9101) (exposer-host "172.17.0.1") (application-port 8700) (application-host "localhost"))
  (initialize-metrics)
  (let* ((prom:*default-registry* *example-registry*)
         (exposer (make-instance 'exposer-acceptor :registry *example-registry* :address exposer-host :port exposer-port))
         (app (make-instance 'application :address application-host :port application-port
                                          :exposer exposer)))
    (tbnl:start app)
    app))

(defun stop (app &key soft)
  (tbnl:stop app :soft soft))

(defmethod tbnl:start ((app application))
  (tbnl:start (application-metrics-exposer app))
  (call-next-method))

(defmethod tbnl:stop ((app application) &key soft)
  (call-next-method)
  (tbnl:stop (application-metrics-exposer app) :soft soft))

(defmethod tbnl:acceptor-log-access ((app application) &key return-code)
  (declare (ignore return-code))
  (unless (mute-access-logs app)
    (call-next-method)))

(defmethod tbnl:acceptor-log-message ((app application) log-level format-string &rest format-arguments)
  (declare (ignore log-level format-string format-arguments))
  (unless (mute-messages-logs app)
    (call-next-method)))

(defmethod tbnl:acceptor-dispatch-request ((app application) request)
  (let ((labels (list (string-downcase (string (tbnl:request-method request)))
                      "example_app")))
    (prom:counter.inc *http-requests-counter* :labels labels)
    (prom:histogram.time (prom:get-metric *http-request-duration* labels)
      "Hello World!")))
