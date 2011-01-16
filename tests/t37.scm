
(define (+ a b)
  (%%cexp (int int -> int) "%0+%1" a b))

(define (thing r)
  (let ((x r.x)
	(z r.z)
	)
    (+ x z)))

(thing {x=1 z=3})
