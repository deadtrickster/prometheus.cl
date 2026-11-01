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

(defun detect-cpu-quota ()
  "CPUs the cgroup CPU quota allows, rounded up, or NIL without a quota.
Checks this process's cgroup, then the root a container sees."
  (destructuring-bind (v2 . v1) (%cgroup-paths)
    (flet ((quota (quota period) (%quota-cpus quota period)))
      (or (loop for dir in (remove nil (list (and v2 (format nil "/sys/fs/cgroup~a" v2)) "/sys/fs/cgroup"))
                for words = (%words (or (%read-file-line (format nil "~a/cpu.max" dir)) ""))
                thereis (and (= 2 (length words)) (quota (first words) (second words))))
          (loop for dir in (remove nil (list (and v1 (format nil "/sys/fs/cgroup/cpu~a" v1)) "/sys/fs/cgroup/cpu"))
                thereis (quota (%read-file-line (format nil "~a/cpu.cfs_quota_us" dir))
                               (%read-file-line (format nil "~a/cpu.cfs_period_us" dir))))))))

(defun detect-affinity-cpus ()
  "CPUs this process may run on, or NIL when unknown."
  #+(and sbcl linux)
  (sb-alien:with-alien ((mask (array (sb-alien:unsigned 8) 128)))
    (when (zerop (sb-alien:alien-funcall
                  (sb-alien:extern-alien "sched_getaffinity"
                                         (function sb-alien:int sb-alien:int sb-alien:unsigned-long
                                                   (* (array (sb-alien:unsigned 8) 128))))
                  0 128 (sb-alien:addr mask)))
      (let ((n (loop for i below 128 sum (logcount (sb-alien:deref mask i)))))
        (and (plusp n) n))))
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
    (max 1 (min configured
                (or (detect-affinity-cpus) configured)
                (or (detect-cpu-quota) configured)))))

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
NIL means one per configured CPU, rounded up to a power of two.")

(defun default-shards ()
  (or *default-shards* *detected-shards*))

(declaim (inline %current-shard-index))
(defun %current-shard-index (mask)
  (declare (type (unsigned-byte 10) mask))
  ;; sched_getcpu returns -1 on failure, which still lands in range
  #+(and sbcl linux)
  (logand (sb-alien:alien-funcall
           (sb-alien:extern-alien "sched_getcpu" (function sb-alien:int)))
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
