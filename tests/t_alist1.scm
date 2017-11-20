;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/alist.scm")

;; this is an attempt to address the "consumes too many registers"
;;  problem with long chains of primapps.  This is a problem with
;;  the bytecode compiler - which has a limited number of registers.
;;  primapps are (now) required to evaluate their arguments in left
;;  to right order.  This causes a huge number of registers to hold
;;  values while waiting for the chain to 'unwind'.

;; since this is somewhat analogous to the tail-recursive accumulator
;;   trick I thought the following macro would help, but actually it
;;   just reverses the order of the initializers...

(defmacro alist/make0
  (alist/make0 (k0 v0) ...)
  -> (alist/make1 (alist:nil) (k0 v0) ...)
  )

(defmacro alist/make1
  (alist/make1 acc)
  -> acc
  (alist/make1 acc (k0 v0) (k1 v1) ...)
  -> (alist/make1 (alist:entry k0 v0 acc) (k1 v1) ...)
  )

;; ... but *this* actually does the trick.  It builds the long chain
;;  using set! and therefore makes the problem go away.  Note: if you
;;  use this macro on something that would otherwise make a literal,
;;  you lose that nice advantage.  So this is only appropriate for
;;  situations where the arguments are not literals (computed,
;;  vectors, records, etc...)

(defmacro alist/make2
  (alist/make2 (k0 v0) ...)
  -> (let ((result (alist:nil)))
       (alist/make3 result (k0 v0) ...)
       result))

(defmacro alist/make3
  (alist/make3 acc) -> acc
  (alist/make3 acc (k0 v0) (k1 v1) ...)
  -> (begin
       (set! acc (alist:entry k0 v0 acc))
       (alist/make3 acc (k1 v1) ...)))

(define numbers2
  (alist/make2
   (0 'zero)
   (1 'one)
   (2 'two)
   (3 'three)
   (4 'four)
   (5 'five)
   (6 'six)
   (7 'seven)
   (8 'eight)
   (9 'nine)
   ))

(printn (alist/lookup numbers2 7))
(printn (alist/lookup numbers2 100))

(alist/push numbers2 100 'one-hundred)

(printn numbers2)
