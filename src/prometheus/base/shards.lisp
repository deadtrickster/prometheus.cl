(in-package #:prometheus)

;;; Per-CPU shards: a writer uses the shard of the CPU it runs on, readers
;;; combine all shards. Writes stay atomic since a thread can be preempted
;;; or migrated mid-update (#20).

(deftype counter-word () #+sbcl 'sb-ext:word #-sbcl '(unsigned-byte 64))

(defmacro %+inf ()
  #+sbcl 'sb-ext:double-float-positive-infinity
  #+ccl 'ccl::double-float-positive-infinity
  #-(or sbcl ccl) 'most-positive-double-float)

(defmacro %-inf ()
  #+sbcl 'sb-ext:double-float-negative-infinity
  #+ccl 'ccl::double-float-negative-infinity
  #-(or sbcl ccl) 'most-negative-double-float)

(declaim (inline %nan-p))
(defun %nan-p (x)
  ;; from the bits: exponent all ones, mantissa not zero. Comparing a NaN
  ;; traps on some SBCLs
  (typecase x
    #+sbcl
    (double-float (let ((hi (sb-kernel:double-float-high-bits x)))
                    (and (= (logand hi #x7ff00000) #x7ff00000)
                         (or (/= 0 (logand hi #xfffff))
                             (/= 0 (sb-kernel:double-float-low-bits x))))))
    #+sbcl
    (single-float (let ((bits (sb-kernel:single-float-bits x)))
                    (and (= (logand bits #x7f800000) #x7f800000)
                         (/= 0 (logand bits #x7fffff)))))
    #-sbcl
    (float (/= x x))
    (t nil)))

(defun %nan-value (value)
  (error 'invalid-value-error :value value :reason "value is NaN"))

(declaim (inline %sat+))
(defun %sat+ (a b)
  "A + B, +/-Inf instead of an overflow trap. Neither bound computation can
overflow: B's sign keeps it inside the double range."
  (declare (type double-float a b))
  (cond ((>= b 0d0) (if (<= a (- most-positive-double-float b)) (+ a b) (%+inf)))
        (t (if (>= a (- most-negative-double-float b)) (+ a b) (%-inf)))))

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
  "CPUs this process can use at once: online CPUs, narrowed by CPU affinity
(cpusets) and the cgroup CPU quota, so a container is not sized by its
host. 4 when the OS cannot tell."
  (%usable-cpus (or (detect-online-cpus) 4) (detect-affinity-cpus) (detect-cpu-quota)))

(defun detect-online-cpus ()
  "Online CPUs from sysconf, or NIL where the constant is not known."
  #+(and sbcl (or linux darwin freebsd))
  (let ((n (sb-alien:alien-funcall
            (sb-alien:extern-alien "sysconf" (function sb-alien:long sb-alien:int))
            ;; _SC_NPROCESSORS_ONLN
            #+linux 84 #+(or darwin freebsd) 58)))
    (and (plusp n) n))
  #+ccl (ccl:cpu-count)
  #-(or (and sbcl (or linux darwin freebsd)) ccl) nil)

(defun %usable-cpus (online affinity quota)
  "ONLINE narrowed by the AFFINITY CPU list and QUOTA, either NIL when
unknown."
  (max 1 (min online
              (if affinity (length affinity) online)
              (or quota online))))

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


;;; Shard slabs. One word array per series, laid out from the cache line
;;; size detected at creation:
;;;
;;;   [ header | gap | stripe 0 | gap | stripe 1 | ... | gap ]
;;;
;;; A stripe's hot words are an integer, a double, then EXTRA words (a
;;; histogram's bucket counts). The array is not line aligned, so a line of
;;; gap after each stripe's last hot word keeps two shards off one line
;;; wherever the array sits. The header holds the geometry, read on every
;;; add: a line of gap keeps it off stripe 0 too, and it is never written,
;;; so a restarted image that detects another line size keeps the slab's.

(deftype slab () #+sbcl '(simple-array sb-ext:word (*)) #-sbcl 'simple-vector)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; CAS on a word array element: built in on newer SBCL, the pair VOP before
  #+sbcl (when (fboundp '(sb-ext:cas aref)) (pushnew :prometheus-cas-aref *features*)))

(defvar *stripe-lines* 1
  "Cache lines of gap between stripes. 2 also keeps shards off a line pair
that adjacent-line prefetch fetches together.")

(defun make-slab (shards &optional (extra 0))
  "A slab of SHARDS stripes, each an integer, a double and EXTRA words, all
zero. One shard elsewhere than SBCL."
  (check-shards shards)
  (let* ((shards #+sbcl shards #-sbcl 1)
         (gap (* *stripe-lines* (max 4 (floor *cache-line-bytes* 8))))
         (hot (+ 2 extra))
         ;; even: the integer and double of a stripe form an aligned pair
         (even (lambda (n) (* 2 (ceiling n 2))))
         (first (funcall even (+ 3 gap)))         ; past the header words
         (stride (funcall even (+ hot -1 gap)))
         (length (+ first (* shards stride)))
         (slab #+sbcl (make-array length :element-type 'sb-ext:word :initial-element 0)
               #-sbcl (make-array length :initial-element 0)))
    (setf (aref slab 0) stride
          (aref slab 1) first
          (aref slab 2) shards)
    #-sbcl (dotimes (s shards) (setf (svref slab (+ first (* s stride) 1)) 0d0))
    slab))

(declaim (inline slab-shards %slab-base %slab-current-base))
(defun slab-shards (slab) (aref (the slab slab) 2))
(defun %slab-base (slab shard)
  "Index of SHARD's integer word."
  (declare (type slab slab) (type (unsigned-byte 10) shard))
  (the fixnum (+ (the fixnum (aref slab 1)) (* shard (the fixnum (aref slab 0))))))
(defun %slab-current-base (slab)
  (%slab-base slab (%current-shard-index (1- (the (integer 1 1024) (slab-shards slab))))))

(declaim (inline %slab-double))
(defun %slab-double (slab base)
  #+sbcl (%w->d (aref (the slab slab) (1+ base)))
  #-sbcl (svref slab (1+ base)))

#+sbcl
(defmacro %cas-slab-double ((old slab base) new-form)
  "Replace the double after the integer at BASE with NEW-FORM, evaluated with
OLD bound to the current double."
  (let ((v (gensym "SLAB")) (i (gensym "BASE")) (bits (gensym "BITS")) (int (gensym "INT")))
    `(let ((,v ,slab) (,i ,base))
       (declare (type slab ,v) (type fixnum ,i))
       (loop
         (let* ((,int 0)
                (,bits (aref ,v (1+ ,i)))
                (,old (%w->d ,bits)))
           (declare (ignorable ,int))
           #+prometheus-cas-aref
           (when (= ,bits (sb-ext:compare-and-swap (aref ,v (1+ ,i)) ,bits (%d->w ,new-form)))
             (return))
           #-prometheus-cas-aref
           ;; no word CAS: swap the 16-byte [integer double] pair, retrying
           ;; when a concurrent integer add got in between
           (let ((,int (aref ,v ,i)))
             (multiple-value-bind (a b) (%cas-pair ,v (ash ,i -1) ,int ,bits ,int (%d->w ,new-form))
               (when (and (= a ,int) (= b ,bits))
                 (return)))))))))

(declaim (inline %slab-add))
(defun %slab-add (slab base n &optional (carry t))
  "Add real N to the integer and double at BASE. Non-negative integers go to
the integer word: with CARRY its overflow moves 2^64 to the double, without
it wraps (uint64). The rest goes to the double, which saturates at +/-Inf.
Allocates nothing for fixnums and doubles. Elsewhere than SBCL the caller
holds a lock."
  (declare (type slab slab) (type fixnum base) (optimize speed))
  (labels ((add-double (d)
             (declare (type double-float d))
             #+sbcl (%cas-slab-double (old slab base) (%sat+ old d))
             #-sbcl (setf (svref slab (1+ base)) (%sat+ (svref slab (1+ base)) d)))
           (add-integer (i)
             (declare (type counter-word i))
             (let ((old #+sbcl
                        ;; ATOMIC-INCF takes a signed delta: the same add modulo 2^64
                        (sb-ext:atomic-incf (aref slab base)
                                            (if (logbitp 63 i) (- i (ash 1 64)) i))
                        #-sbcl
                        (prog1 (svref slab base)
                          (setf (svref slab base) (ldb (byte 64 0) (+ (svref slab base) i))))))
               (declare (type counter-word old))
               (when (and carry (< (- (ldb (byte 64 0) -1) old) i))
                 (add-double 1.8446744073709552d19)))))
    (declare (inline add-integer add-double))
    ;; generic COERCE returns a boxed double
    (typecase n
      ((integer 0 #.most-positive-fixnum) (add-integer n))
      (counter-word (add-integer n))
      (double-float (add-double n))
      (single-float (add-double (coerce n 'double-float)))
      (t (add-double (coerce n 'double-float)))))
  nil)

(defun slab-value (slab)
  "Integer plus double summed over shards: an integer while only
non-negative integers were added."
  (let ((integer 0)
        (double 0d0)
        (any-double nil))
    (dotimes (s (slab-shards slab))
      (let ((base (%slab-base slab s)))
        (incf integer (aref slab base))
        (let ((d (%slab-double slab base)))
          (unless (zerop d) (setf any-double t))
          (setf double (%sat+ double d)))))
    (if any-double (+ integer double) integer)))

(defun slab-extra-sum (slab k)
  "Extra word K summed over shards."
  (let ((sum 0))
    (dotimes (s (slab-shards slab) sum)
      (incf sum (aref slab (+ (%slab-base slab s) 2 k))))))

(defun slab-reset (slab)
  "Zero every stripe. Not atomic against concurrent adds."
  (let ((stride (aref slab 0)))
    (dotimes (s (slab-shards slab))
      (let ((base (%slab-base slab s)))
        (loop for i from base below (+ base stride) do (setf (aref slab i) 0))
        #-sbcl (setf (svref slab (1+ base)) 0d0))))
  slab)

;;; counters: a slab without extra words
(declaim (inline make-counter-cells counter-cells-add counter-cells-value counter-cells-reset))
(defun make-counter-cells (shards) (make-slab shards))
(defun counter-cells-add (cells n &optional (carry t))
  "Add N to the current CPU's stripe."
  (%slab-add cells (%slab-current-base cells) n carry))
(defun counter-cells-value (cells) (slab-value cells))
(defun counter-cells-reset (cells) (slab-reset cells))
