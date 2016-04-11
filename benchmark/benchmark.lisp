(ql:quickload :iolib)

(defmacro timing% (repeat &body body)
  `(multiple-value-bind (sec1 nsec1) (iolib.syscalls:clock-gettime iolib.syscalls:clock-monotonic)
     (incf nsec1(* sec1 1000000000))
     (loop repeat ,repeat
           do
              ,@body)
     (multiple-value-bind (sec2 nsec2) (iolib.syscalls:clock-gettime iolib.syscalls:clock-monotonic)
       (incf nsec2 (* sec2 1000000000))
       (- nsec2 nsec1))))

(defun reduce-threads (threads)
  (reduce (lambda (v x)
            (+ v (bt:join-thread x)))
          threads
          :initial-value 0))

(defmacro run-threads (threads repeat body)
  `(loop repeat ,threads
         collect
            (bt:make-thread (lambda ()
                              (timing% ,repeat
                                ,@body)))))

(defmacro ptiming ((&key (threads 1) (repeat 10000) (warm 10)) &body body)
  `(coerce (/ (if (= 1 ,threads)
                  (progn
                    (if ,warm
                        (timing% ,warm
                          ,@body))
                    (timing% ,repeat
                      ,@body))
                  (reduce-threads
                          (progn (if ,warm
                                     (run-threads ,warm ,repeat ,body))
                                 (run-threads ,threads ,repeat ,body))))
              (* ,threads ,repeat))
           'double-float))

(defun test-counter (threads repeat warm)
  (let* ((c (prom:make-counter :name "qwe" :help "qwe" :labels '("label1" "label2" "label3") :registry nil))
         (m (prom:get-metric c '("1" "2" "3"))))
    (prog1 (ptiming (:threads threads :repeat repeat :warm warm)
             (prom:counter.inc m)
             ;; (prom:counter.inc c :labels '("1" "2" "3"))
             ;; (prom:counter.inc c :labels '("4" "5" "6"))
             ;; (prom:counter.inc c :labels '("7" "8" "9"))
             )
      (print (prom:metric-value m)))))

(defun test-int-counter (threads repeat warm)
  (let* ((c (prom::make-int-counter :name "qwe" :help "qwe" :labels '("label1" "label2" "label3") :registry nil))
         (m (prom:get-metric c '("1" "2" "3"))))
    (prog1 (ptiming (:threads threads :repeat repeat :warm warm)
             (prom:counter.inc m)
             ;; (prom:counter.inc c :labels '("1" "2" "3"))
             ;; (prom:counter.inc c :labels '("4" "5" "6"))
             ;; (prom:counter.inc c :labels '("7" "8" "9"))
             )
      (print (prom:metric-value m)))))


(defun test-int-counter-no-label (threads repeat warm)
  (let* ((c (prom::make-int-counter :name "qwe" :help "qwe" :registry nil)))
    (prog1 (ptiming (:threads threads :repeat repeat :warm warm)
             (prom:counter.inc c))
      (print (prom:metric-value c)))))


(defun test-gauge (threads repeat warm)
  (let* ((c (prom:make-gauge :name "qwe" :help "qwe" :labels '("label1" "label2" "label3") :registry nil))
         (m (prom:get-metric c '("1" "2" "3"))))
    (prog1 (ptiming (:threads threads :repeat repeat :warm warm)
             (prom:gauge.set m 10))
      (print (prom:metric-value m)))))

(defun test-histogram (threads repeat warm)
  (let* ((h (prom:make-histogram :name "qwe" :buckets '(2 4 6) :value '(4.5 1 0.5) :registry nil))
         (m (prom:get-metric h nil)))
    (prog1 (ptiming (:threads threads :repeat repeat :warm warm)
             ;; (prom:histogram.observe m 1)
             ;; (prom:histogram.observe m 2)
             ;; (prom:histogram.observe m 3)
             (prom:histogram.observe m 7))
      (print (prom:metric-value m)))))

(defun test-summary (threads repeat warm)
  (let* ((s (prom:make-summary :name "qwe" :value 12 :count 2 :registry nil))
         (m (prom:get-metric s nil)))
    (prog1 (ptiming (:threads threads :repeat repeat :warm warm)
             (prom:summary.observe m 2))
      (print (prom:metric-value m)))))
