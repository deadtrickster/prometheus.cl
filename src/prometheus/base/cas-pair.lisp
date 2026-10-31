(in-package #:prometheus)

;;; 16-byte CAS and load on two adjacent elements of a 64-bit specialized
;;; vector. SBCL only exposes a pair CAS for SIMPLE-VECTOR. Other platforms
;;; use a lock instead, see %WITH-SKETCH-LOCKED.

#+(and sbcl x86-64)
(progn
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (pushnew :prometheus-cas-pair *features*))

  (sb-c:defknown %cas-pair ((simple-array * (*)) sb-int:index
                            sb-ext:word sb-ext:word sb-ext:word sb-ext:word)
      (values sb-ext:word sb-ext:word) ())

  ;; Pair I is elements 2I, 2I+1. Returns the pair found there; the swap
  ;; happened iff it equals (OLD1 OLD2). I must arrive untagged.
  (sb-c:define-vop (%cas-pair)
    (:translate %cas-pair)
    (:policy :fast-safe)
    (:args (v :scs (sb-vm::descriptor-reg))
           (i :scs (sb-vm::unsigned-reg))
           (o1 :scs (sb-vm::unsigned-reg) :target rax)
           (o2 :scs (sb-vm::unsigned-reg) :target rdx)
           (n1 :scs (sb-vm::unsigned-reg) :target rbx)
           (n2 :scs (sb-vm::unsigned-reg) :target rcx))
    (:arg-types * sb-vm::unsigned-num sb-vm::unsigned-num sb-vm::unsigned-num
                sb-vm::unsigned-num sb-vm::unsigned-num)
    (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::rsi-offset :from (:argument 0)) rsi)
    (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::rax-offset :from (:argument 2) :to (:result 0)) rax)
    (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::rdx-offset :from (:argument 3) :to (:result 1)) rdx)
    (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::rbx-offset :from (:argument 4)) rbx)
    (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::rcx-offset :from (:argument 5)) rcx)
    (:results (r1 :scs (sb-vm::unsigned-reg)) (r2 :scs (sb-vm::unsigned-reg)))
    (:result-types sb-vm::unsigned-num sb-vm::unsigned-num)
    (:generator 10
      (sb-vm::move rsi i)
      (sb-assem:inst sb-vm::shl rsi 4)
      (sb-assem:inst sb-vm::add rsi v)
      (sb-vm::move rax o1)
      (sb-vm::move rdx o2)
      (sb-vm::move rbx n1)
      (sb-vm::move rcx n2)
      ;; lock cmpxchg16b [rsi + data offset - lowtag]
      (sb-assem:inst sb-assem::.byte #xf0 #x48 #x0f #xc7 #x4e
                     (- (* sb-vm:vector-data-offset sb-vm:n-word-bytes) sb-vm:other-pointer-lowtag))
      (sb-vm::move r1 rax)
      (sb-vm::move r2 rdx)))

  (sb-c:defknown %load-pair ((simple-array double-float (*)) sb-int:index)
      (values double-float double-float) (sb-c:flushable))

  ;; One aligned 16-byte load, split into two doubles without leaving the
  ;; XMM registers. Atomic on CPUs with AVX (Intel SDM 9.1.1, AMD APM 7.3.2),
  ;; check **ATOMIC-PAIR-LOAD** first. SSE2 only.
  (sb-c:define-vop (%load-pair)
    (:translate %load-pair)
    (:policy :fast-safe)
    (:args (v :scs (sb-vm::descriptor-reg))
           (i :scs (sb-vm::unsigned-reg)))
    (:arg-types * sb-vm::unsigned-num)
    (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::rsi-offset :from (:argument 0)) rsi)
    (:temporary (:sc sb-vm::double-reg :offset 15 :to (:result 1)) xmm15)
    (:temporary (:sc sb-vm::double-reg :offset 14 :to (:result 1)) xmm14)
    (:results (r1 :scs (sb-vm::double-reg)) (r2 :scs (sb-vm::double-reg)))
    (:result-types sb-vm::double-float sb-vm::double-float)
    (:generator 3
      (sb-vm::move rsi i)
      (sb-assem:inst sb-vm::shl rsi 4)
      (sb-assem:inst sb-vm::add rsi v)
      ;; movdqa xmm15, [rsi + data offset - lowtag]
      (sb-assem:inst sb-assem::.byte #x66 #x44 #x0f #x6f #x7e
                     (- (* sb-vm:vector-data-offset sb-vm:n-word-bytes) sb-vm:other-pointer-lowtag))
      (sb-assem:inst sb-assem::.byte #x45 #x0f #x12 #xf7)      ; movhlps xmm14, xmm15
      (sb-assem:inst sb-x86-64-asm::movapd r1 xmm15)
      (sb-assem:inst sb-x86-64-asm::movapd r2 xmm14)))

  (sb-c:defknown %pair-add ((simple-array double-float (*)) sb-int:index
                            (simple-array double-float (*)) sb-int:index)
      (values) ())

  ;; Pair DI of DST += pair SI of SRC, both lanes in one ADDPD. The SRC read
  ;; is one aligned 16-byte load, atomic under the same condition as
  ;; %LOAD-PAIR. DST must not be written concurrently.
  (sb-c:define-vop (%pair-add)
    (:translate %pair-add)
    (:policy :fast-safe)
    (:args (dst :scs (sb-vm::descriptor-reg))
           (di :scs (sb-vm::unsigned-reg))
           (src :scs (sb-vm::descriptor-reg))
           (si :scs (sb-vm::unsigned-reg)))
    (:arg-types * sb-vm::unsigned-num * sb-vm::unsigned-num)
    (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::rsi-offset :from (:argument 0)) rsi)
    (:temporary (:sc sb-vm::unsigned-reg :offset sb-vm::rdi-offset :from (:argument 0)) rdi)
    (:temporary (:sc sb-vm::double-reg :offset 15) xmm15)
    (:generator 4
      xmm15
      (sb-vm::move rsi si)
      (sb-assem:inst sb-vm::shl rsi 4)
      (sb-assem:inst sb-vm::add rsi src)
      (sb-vm::move rdi di)
      (sb-assem:inst sb-vm::shl rdi 4)
      (sb-assem:inst sb-vm::add rdi dst)
      (let ((disp (- (* sb-vm:vector-data-offset sb-vm:n-word-bytes) sb-vm:other-pointer-lowtag)))
        (sb-assem:inst sb-assem::.byte #x66 #x44 #x0f #x6f #x7e disp)    ; movdqa xmm15, [rsi+d]
        (sb-assem:inst sb-assem::.byte #x66 #x44 #x0f #x58 #x7f disp)    ; addpd  xmm15, [rdi+d]
        (sb-assem:inst sb-assem::.byte #x66 #x44 #x0f #x7f #x7f disp)))) ; movdqa [rdi+d], xmm15

  (defun %detect-atomic-pair-load ()
    ;; CPUID.1:ECX bit 28 AVX, bit 27 OSXSAVE
    (let ((ecx (nth-value 2 (sb-vm::%cpu-identification 1 0))))
      (and (logbitp 28 ecx) (logbitp 27 ecx) t)))

  (sb-ext:defglobal **atomic-pair-load** nil)
  (setf **atomic-pair-load** (%detect-atomic-pair-load))

  (pushnew (lambda () (setf **atomic-pair-load** (%detect-atomic-pair-load)))
           sb-ext:*init-hooks*))
