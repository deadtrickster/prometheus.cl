(in-package #:prometheus)

;;; 16-byte CAS on two adjacent elements of a 64-bit specialized vector
;;; (LOCK CMPXCHG16B). SBCL only exposes a pair CAS for SIMPLE-VECTOR.
;;; Other platforms use a lock instead, see %WITH-SKETCH-LOCKED.

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
      (sb-vm::move r2 rdx))))
