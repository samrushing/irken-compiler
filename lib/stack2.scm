;; -*- Mode: Irken -*-

(define (stack-class)
  (define (push self x)
    (set! self.s (list:cons x self.s)))
  (define (pop self)
    (match self.s with
      () -> (error "stack underflow")
      (hd . tl) -> (let ((result hd))
		     (set! self.s tl)
		     result)))
  (define (get self) self.s)
  (define (len self) (length self.s))
  (let ((methods {push=push pop=pop get=get len=len}))
    (lambda ()
      {o=methods self={s='()}}))
  )

(define new-stack (stack-class))
