;; -*- Mode: Irken -*-

;; experimenting with a new imperative object design.
;;  here, we use a datatype wrapper for the object,
;;  which is unwrapped at method invocation time.
;; sadly, this can't (really) be optimized away - if
;;  we ever get around to doing the single-arm-datatype
;;  optimization, this will still call 'id'.

(datatype stack-ob
  (:t {s=(list 'a) len=int})
  )

(define (stack-class)

  (define (push self x)
    (set! self.s (list:cons x self.s)))

  (define (pop self)
    (match self.s with
      () -> (error "stack underflow")
      (hd . tl)
      -> (let ((result hd))
           (set! self.s tl)
           result)))

  (define un
    (stack-ob:t ob) -> ob
    )

  (let ((methods {push=push pop=pop un=un}))
    (lambda ()
      {o=methods self=(stack-ob:t {s='() len=0})}))
  )

(define new-stack (stack-class))

(include "lib/core.scm")
(include "lib/pair.scm")

(defmacro obcall
  (obcall ob method arg0 ...)
  -> ((%%splice
       (%%attr (%%attr ob o) method)
       ((%%attr (%%attr ob o) un) (%%attr ob self)))
      arg0 ...)
  )

(let ((s (new-stack)))
  (s@push 1)
  (s@push 2)
  (s@push 3)
  (s@push 4)
  (s@push 5)
  (printn (s@pop))
  (printn (s@pop))
  (printn (s@pop))
  (printn (s@pop))
  (printn (s@pop))
  )

;;(printn (s.o.pop (s.o.un s.self)))
