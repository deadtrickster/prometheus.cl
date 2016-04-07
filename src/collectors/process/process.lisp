(in-package #:prometheus.process)

(defconstant +lisp-epoch-start+ 2208988800)

(defclass process-collector (prom:collector)
  ((page-size :initarg :page-size :reader page-size)
   (ticks :initarg :ticks :reader ticks)
   (start-time :initarg :start-time :reader start-time)))

(defun read-stat ()
  (with-open-file (stream "/proc/self/stat")
    (split-sequence #\Space
                    (read-line stream)
                    :remove-empty-subseqs t)))

(defun get-btime ()
  (when-let ((info-line
              (with-open-file (stream "/proc/stat")
                (loop
                  as line = (read-line stream nil) do
                     (unless line
                       (return))
                     (if (starts-with-subseq "btime" line)
                         (return line))))))
    (parse-integer (elt (split-sequence #\Space info-line :remove-empty-subseqs t) 1))))

(defun get-ticks ()
  (sysconf +_sc_clk_tck+))

(defun get-start-time ()
  (let ((stat (read-stat)))
    (+ (round (/ (parse-integer (elt stat 21)) (get-ticks))) (get-btime))))

(defun make-process-collector (&key (namespace "") (name "process_collector") (registry prom:*default-registry*))
  (let ((collector (make-instance 'process-collector :namespace namespace
                                                     :name name
                                                     :page-size (or (ignore-errors
                                                                     (sysconf +_sc_pagesize+))
                                                                    4096)
                                                     :ticks (ignore-errors
                                                             (get-ticks))
                                                     :start-time (ignore-errors
                                                                  (get-start-time)))))
    (when registry
      (prom:register collector registry))
    collector))

(defun get-open-fds-count ()
  (length (cl-fad:list-directory "/proc/self/fd")))

(defun get-max-fds-count ()
  (when-let ((info-line
              (with-open-file (stream "/proc/self/limits")
                (loop
                  as line = (read-line stream nil) do
                     (unless line
                       (return))
                     (if (starts-with-subseq "Max open file" line)
                         (return line))))))
    (elt (split-sequence #\Space info-line :remove-empty-subseqs t) 3)))

(defmethod prom:collect ((pc process-collector) cb)
  (when (and (start-time pc)
             (cl-fad:file-exists-p "/proc/self"))
    ;;fds
    (funcall cb (prom:make-gauge :name (prom:collector-metric-name pc "process_open_fds")
                                 :help "Number of open file descriptors."
                                 :value (get-open-fds-count)
                                 :registry nil)))
  (funcall cb (prom:make-gauge :name (prom:collector-metric-name pc "process_max_fds")
                               :help "Maximum number of open file descriptors."
                               :value (parse-integer (get-max-fds-count))
                               :registry nil))


  (funcall cb (prom:make-gauge :name (prom:collector-metric-name pc "process_start_time_seconds")
                               :help "Start time of the process since unix epoch in seconds."
                               :value (start-time pc)
                               :registry nil))
  (funcall cb (prom:make-gauge :name (prom:collector-metric-name pc "process_uptime_seconds")
                               :help "Process uptime in seconds."
                               :value (- (get-universal-time) (start-time pc) +lisp-epoch-start+)
                               :registry nil))

  ;;stat
  (let ((stat (read-stat)))

    (funcall cb (prom:make-gauge :name (prom:collector-metric-name pc "process_threads_total")
                                 :help "Process Threads count."
                                 :value (parse-integer (elt stat 19))
                                 :registry nil))

    (funcall cb (prom:make-gauge :name (prom:collector-metric-name pc "process_virtual_memory_bytes")
                                 :help "Virtual memory size in bytes."
                                 :value (parse-integer (elt stat 22))
                                 :registry nil))
    (funcall cb (prom:make-gauge :name (prom:collector-metric-name pc "process_resident_memory_bytes")
                                 :help "Resident memory size in bytes."
                                 :value (* (page-size pc) (parse-integer (elt stat 23)))
                                 :registry nil))

    (let ((utime (/ (parse-integer (elt stat 13)) (ticks pc)))
          (stime (/ (parse-integer (elt stat 14)) (ticks pc)))
          (c (prom:make-counter :name (prom:collector-metric-name pc "process_cpu_seconds")
                                :help "Process CPU seconds."
                                :labels '("time")
                                :registry nil)))
      (prom:counter.inc c :value (coerce utime 'double-float) :labels '("utime"))
      (prom:counter.inc c :value (coerce stime 'double-float) :labels '("stime"))
      (funcall cb c)
      (funcall cb (prom:make-counter :name (prom:collector-metric-name pc "process_cpu_seconds_total")
                                     :help "Process CPU seconds total."
                                     :value (coerce (+ utime stime) 'double-float)
                                     :registry nil)))))
