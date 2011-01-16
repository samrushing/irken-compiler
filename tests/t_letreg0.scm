
(define (printn x)
  (%%cexp ('a -> undefined) "dump_object (%0, 0); fprintf (stdout, \"\\n\")" x))

(define (+ a b)
  (%%cexp (int int -> int) "%0+%1" a b))

(define (make-counter)
  (let ((val 0))
    (define (next)
      (let ((r val))
	(set! val (+ val 1))
	r)
      )
    next
    ))

(let ((c (make-counter)))
  (printn (c))
  (printn (c))
  )
