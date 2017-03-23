;; -*- Mode: Irken -*-

;; this code aggravates an issue with letreg and register allocation.

;; in cps/c-let-reg:
;; the original line:
;;
;;  (compile tail? (car inits) lenv (add-free-regs k regs))
;;
;; would lead to the target getting stomped on by the first
;;   letreg variable.  strange and rare problem that only shows
;;   up in the bytecode compiler.
;;
;; this extra move seems to fix things:
;;
;;   (compile tail? (car inits) lenv 
;;            (cont (merge-sets regs (k/free k))
;;                  (lambda (reg)
;;                    (insn:move reg -1 k))))

(define (vector-length v)
  (%backend (c llvm)
    (%%cexp
     ((vector 'a) -> int)
     "(%0 == (object*) TC_EMPTY_VECTOR) ? 0 : GET_TUPLE_LENGTH(*%0)" v))
  (%backend bytecode
    (%%cexp ((vector 'a) -> int) "vlen" v))
  )

(define thing
  (let ((x #(1 2 3 4)))
    { a=(vector-length x) b=x }
    ))

thing
