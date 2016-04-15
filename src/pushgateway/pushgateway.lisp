(in-package #:prometheus.pushgateway)

(define-constant +default-pushgateway-address+ "localhost:9091" :test #'equal)

(defun http-request (gateway method job grouping-key content-type body)
  (unless (and (listp grouping-key)
               (evenp (length grouping-key)))
    (error 'prom:invalid-labels-error :actual grouping-key :expected 'plist))
  (multiple-value-bind (body status-code headers)
      (drakma:http-request (format nil "http://~a/metrics/job/~a~:[~;~:*/~{~a~^/~}~]" gateway job grouping-key)
                           :method method
                           :content-type content-type
                           :content body)
    (if (/= status-code 202)
        (error "Error talking to pushgateway. Response Code: ~a, body: ~a, headers: ~a" status-code body headers))))

(defun push (job &key (gateway +default-pushgateway-address+)
                      (registry prom:*default-registry*)
                      (grouping-key))
  (http-request gateway :post job grouping-key prom.text:+content-type+ (prom.text:marshal registry)))

(defun replace (job &key (gateway +default-pushgateway-address+)
                         (registry prom:*default-registry*)
                         (grouping-key))
  (http-request gateway :put job grouping-key prom.text:+content-type+ (prom.text:marshal registry)))

(defun delete (job &key (gateway +default-pushgateway-address+)
                        (grouping-key))
  (http-request gateway :delete job grouping-key prom.text:+content-type+ nil))
