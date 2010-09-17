;; -*- Mode: Irken -*-

(include "lib/core.scm")

(define (other-fun)
  {d=2 e=#t f="there"})

(define thing
  0 f -> {d=1 e=#f f="hello" a=7}
  _ f -> (let ((r (other-fun)))
	 (%rextend/a r 34))
  )

(define (thing2)
  (let ((r {a=0 b=1 c=2}))
    (%rextend/b r 12)))

(printn (thing 2 other-fun))
(printn (thing2))


