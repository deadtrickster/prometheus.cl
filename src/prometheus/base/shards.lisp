(in-package #:prometheus)

;;; Per-CPU shards: a writer uses the shard of the CPU it runs on, readers
;;; combine all shards. Writes stay atomic since a thread can be preempted
;;; or migrated mid-update (#20).

(deftype counter-word () #+sbcl 'sb-ext:word #-sbcl '(unsigned-byte 64))

(defun check-shards (shards)
  (unless (and (typep shards '(integer 1 1024)) (= 1 (logcount shards)))
    (error 'invalid-value-error :value shards :reason "shards should be a power of two in [1, 1024]")))

(defun %read-file-line (path)
  (ignore-errors (with-open-file (in path :if-does-not-exist nil)
                   (and in (read-line in nil)))))

(defun %words (line &optional (separators '(#\Space #\Tab)))
  (let ((words '()) (start nil))
    (loop for i from 0 to (length line)
          for c = (if (< i (length line)) (char line i) (first separators))
          do (cond ((member c separators) (when start (push (subseq line start i) words) (setf start nil)))
                   ((null start) (setf start i))))
    (nreverse words)))

(defun %cgroup-paths ()
  "(V2-PATH . V1-CPU-PATH) of this process from /proc/self/cgroup."
  (let ((v2 nil) (v1 nil))
    (ignore-errors
     (with-open-file (in "/proc/self/cgroup" :if-does-not-exist nil)
       (when in
         (loop for line = (read-line in nil) while line
               do (let* ((a (position #\: line)) (b (and a (position #\: line :start (1+ a)))))
                    (when b
                      (let ((controllers (subseq line (1+ a) b)) (path (subseq line (1+ b))))
                        (cond ((string= controllers "") (setf v2 path))
                              ((member "cpu" (%words controllers '(#\,)) :test #'string=)
                               (setf v1 path))))))))))
    (cons v2 v1)))

(defun %quota-cpus (quota period)
  "CPUs a QUOTA per PERIOD (strings, as cgroups write them) allows, rounded
up. NIL for no quota: \"max\" in v2, -1 in v1."
  (let ((q (and quota (ignore-errors (parse-integer quota))))
        (p (and period (ignore-errors (parse-integer period)))))
    (and q p (plusp q) (plusp p) (ceiling q p))))

(defun %cgroup-dirs (root path)
  "ROOT/PATH and each of its ancestors up to ROOT, leaf first."
  (let ((dirs (list root)) (prefix root))
    (dolist (part (%words (or path "") '(#\/)) dirs)
      (setf prefix (format nil "~a/~a" prefix part))
      (push prefix dirs))))

(defun detect-cpu-quota ()
  "CPUs the tightest cgroup CPU quota on this process's cgroup or its
ancestors allows, rounded up, or NIL without one."
  (destructuring-bind (v2 . v1) (%cgroup-paths)
    (let ((quotas
            (append
             (loop for dir in (%cgroup-dirs "/sys/fs/cgroup" v2)
                   for words = (%words (or (%read-file-line (format nil "~a/cpu.max" dir)) ""))
                   collect (and (= 2 (length words)) (%quota-cpus (first words) (second words))))
             (loop for dir in (%cgroup-dirs "/sys/fs/cgroup/cpu" v1)
                   collect (%quota-cpus (%read-file-line (format nil "~a/cpu.cfs_quota_us" dir))
                                        (%read-file-line (format nil "~a/cpu.cfs_period_us" dir)))))))
      (let ((found (remove nil quotas)))
        (and found (reduce #'min found))))))

(defun detect-affinity-cpus ()
  "IDs of the CPUs this process may run on, ascending, or NIL when unknown."
  #+(and sbcl linux)
  (sb-alien:with-alien ((mask (array (sb-alien:unsigned 8) 128)))
    (when (zerop (sb-alien:alien-funcall
                  (sb-alien:extern-alien "sched_getaffinity"
                                         (function sb-alien:int sb-alien:int sb-alien:unsigned-long
                                                   (* (array (sb-alien:unsigned 8) 128))))
                  0 128 (sb-alien:addr mask)))
      (loop for cpu below 1024
            when (logbitp (mod cpu 8) (sb-alien:deref mask (floor cpu 8)))
              collect cpu)))
  #-(and sbcl linux) nil)

(defun detect-cpu-count ()
  "CPUs this process can use at once: configured CPUs, narrowed by CPU
affinity (cpusets) and the cgroup CPU quota, so a container is not sized
by its host."
  (let ((configured
          #+(and sbcl linux)
          (let ((n (sb-alien:alien-funcall
                    (sb-alien:extern-alien "sysconf" (function sb-alien:long sb-alien:int))
                    83)))               ; _SC_NPROCESSORS_CONF
            (if (plusp n) n 16))
          #-(and sbcl linux) 16))
    (%usable-cpus configured (detect-affinity-cpus) (detect-cpu-quota))))

(defun %usable-cpus (configured affinity quota)
  "CONFIGURED narrowed by the AFFINITY CPU list and QUOTA, either NIL when
unknown."
  (max 1 (min configured
              (if affinity (length affinity) configured)
              (or quota configured))))

(defun round-up-shards (n)
  (min 1024 (ash 1 (integer-length (1- (max 1 n))))))

(defvar *detected-shards* (round-up-shards (detect-cpu-count)))

#+sbcl
(pushnew (lambda () (setf *detected-shards* (round-up-shards (detect-cpu-count))))
         sb-ext:*init-hooks*)

(defun detect-cache-line-bytes ()
  "L1 data cache line size, 64 when it cannot be read."
  (let ((n (or (ignore-errors
                (with-open-file (in "/sys/devices/system/cpu/cpu0/cache/index0/coherency_line_size")
                  (parse-integer (read-line in))))
               64)))
    (if (and (<= 16 n 1024) (= 1 (logcount n))) n 64)))

(defvar *cache-line-bytes* (detect-cache-line-bytes))

#+sbcl
(pushnew (lambda () (setf *cache-line-bytes* (detect-cache-line-bytes)))
         sb-ext:*init-hooks*)

(defvar *default-shards* nil
  "Shards per series for sharded metrics, a power of two in [1, 1024].
NIL means one per CPU the process can use, rounded up to a power of two.")

(defun default-shards ()
  (or *default-shards* *detected-shards*))

(deftype cpu-slots () '(simple-array (unsigned-byte 10) (1024)))

(defun make-cpu-slots (cpus)
  "CPU id -> shard slot. The CPUS a process may use get dense slots, so a
sparse set like {0, 4, 8, 12} still spreads over 4 shards. Others keep
their id."
  (let ((slots (make-array 1024 :element-type '(unsigned-byte 10))))
    (dotimes (i 1024) (setf (aref slots i) i))
    (loop for cpu in cpus for slot from 0
          do (setf (aref slots cpu) (logand slot 1023)))
    slots))

#+sbcl (sb-ext:defglobal **cpu-slots** nil)
#+sbcl (setf **cpu-slots** (make-cpu-slots (detect-affinity-cpus)))
#+sbcl (pushnew (lambda () (setf **cpu-slots** (make-cpu-slots (detect-affinity-cpus))))
                sb-ext:*init-hooks*)

(declaim (inline %current-shard-index))
(defun %current-shard-index (mask)
  (declare (type (unsigned-byte 10) mask))
  #+(and sbcl linux)
  (logand (aref (the cpu-slots **cpu-slots**)
                ;; sched_getcpu returns -1 on failure, which still lands in range
                (logand (sb-alien:alien-funcall
                         (sb-alien:extern-alien "sched_getcpu" (function sb-alien:int)))
                        1023))
          mask)
  #+(and sbcl (not linux))
  (logand (sb-thread:thread-os-tid sb-thread:*current-thread*) mask)
  #-sbcl (progn mask 0))

#+sbcl
(progn
  (declaim (inline %d->w %w->d))
  (defun %d->w (d)
    (declare (double-float d))
    (ldb (byte 64 0) (sb-kernel:double-float-bits d)))
  (defun %w->d (w)
    (declare (type sb-ext:word w))
    (sb-kernel:make-double-float (- (ldb (byte 32 32) w) (if (logbitp 63 w) (ash 1 32) 0))
                                 (ldb (byte 32 0) w))))

#+sbcl
(defmacro %cas-double-bits ((old accessor object) new-form &key (when t))
  "Replace the double stored as bits in (ACCESSOR OBJECT) with NEW-FORM,
evaluated with OLD bound to the current double, as long as WHEN holds."
  (let ((bits (gensym "BITS")))
    `(loop
       (let* ((,bits (,accessor ,object))
              (,old (%w->d ,bits)))
         (declare (ignorable ,old))
         (unless ,when (return))
         (when (= ,bits (sb-ext:compare-and-swap (,accessor ,object) ,bits (%d->w ,new-form)))
           (return))))))
