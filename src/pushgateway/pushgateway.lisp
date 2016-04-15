(in-package #:prometheus.pushgateway)

(define-constant +default-pushgateway-address+ "localhost:9091" :test #'equal)

(defun escape (str &optional (safe ""))
  "URI encodes/escapes the given string."
  (with-output-to-string (s)
    (loop for c across (flexi-streams:string-to-octets str :external-format :utf-8)
          do (if (or (find (code-char c) safe)
                     (<= 48 c 57)
                     (<= 65 c 90)
                     (<= 97 c 122)
                     (find c '(45 95 46 126)))
              (write-char (code-char c) s)
              (format s "%~2,'0x" c)))))

(defun check-no-/ (string)
  (and (not (find #\/ string))
       (not (search "%2f" (string-downcase string)))))

(defun validate-job-name (thing)
  (unless (stringp thing)
    (error 'prom:invalid-label-value-error :value thing :reason "job name is not a string"))
  (unless (check-no-/ thing)
    (error 'prom:invalid-label-value-error :value thing :reason "job name contains / or %2f")))

(defun validate-label-name (thing)
  (prom::check-label-name thing))

(defun validate-label-value (thing)
  (unless (stringp thing)
    (error 'prom:invalid-label-value-error :value thing :reason "label value is not a string"))
  (unless (check-no-/ thing)
    (error 'prom:invalid-label-value-error :value thing :reason "label value contains / or %2f")))

(defun validate-grouping-key (grouping-key)
  (unless (and (listp grouping-key)
               (evenp (length grouping-key)))
    (error 'prom:invalid-labels-error :actual grouping-key :expected 'plist))
  (loop for (label-name label-value) on grouping-key by #'cddr
        do
           (validate-label-name label-name)
           (validate-label-value label-value)))

(defun escape-job-name (job)
  (escape job))

(defun escape-grouping-key (grouping-key)
  (mapcar #'escape grouping-key))

(defun http-request (gateway method job grouping-key content-type body)
  (validate-job-name job)
  (validate-grouping-key grouping-key)
  (multiple-value-bind (body status-code headers)
      (drakma:http-request (format nil "http://~a/metrics/job/~a~:[~;~:*/~{~a~^/~}~]"
                                   gateway
                                   (escape-job-name job)
                                   (escape-grouping-key grouping-key))
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
