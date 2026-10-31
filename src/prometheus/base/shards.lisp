(in-package #:prometheus)

;;; Per-CPU shards: a writer uses the shard of the CPU it runs on, readers
;;; combine all shards. Writes stay atomic since a thread can be preempted
;;; or migrated mid-update (#20).

(deftype counter-word () #+sbcl 'sb-ext:word #-sbcl '(unsigned-byte 64))

(defun check-shards (shards)
  (unless (and (typep shards '(integer 1 1024)) (= 1 (logcount shards)))
    (error 'invalid-value-error :value shards :reason "shards should be a power of two in [1, 1024]")))

(defun detect-cpu-count ()
  #+(and sbcl linux)
  (let ((n (sb-alien:alien-funcall
            (sb-alien:extern-alien "sysconf" (function sb-alien:long sb-alien:int))
            83)))                       ; _SC_NPROCESSORS_CONF
    (if (plusp n) n 16))
  #-(and sbcl linux) 16)

(defun round-up-shards (n)
  (min 1024 (ash 1 (integer-length (1- (max 1 n))))))

(defvar *detected-shards* (round-up-shards (detect-cpu-count)))

#+sbcl
(pushnew (lambda () (setf *detected-shards* (round-up-shards (detect-cpu-count))))
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
