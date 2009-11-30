
(define (printn x)
  (%%cexp ('a -> undefined) "dump_object (%s, 0); fprintf (stdout, \"\\n\")" x))

(define (+ a b)
  (%%cexp (int int -> int) "%s+%s" a b))

(define (z x)
  (+ x.a 1))

(let ((y {a=#\A b=#\B}))
  (printn (z y))
  )
