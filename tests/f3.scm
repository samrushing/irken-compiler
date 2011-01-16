
(define (printn x)
  (%%cexp ('a -> undefined) "dump_object (%0, 0); fprintf (stdout, \"\\n\")" x))

(define (+ a b)
  (%%cexp (int int -> int) "%0+%1" a b))

(define (z x)
  (+ x.a 1))

(let ((y {a=#\A b=#\B}))
  (printn (z y))
  )
