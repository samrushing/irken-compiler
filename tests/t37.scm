
(define (+ a b)
  (%%cexp (int int -> int) "%s+%s" a b))

(define (thing r)
  (let ((x r.x)
	(z r.z)
	)
    (+ x z)))

(thing {x=1 z=3})
