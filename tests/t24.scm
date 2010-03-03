
(define (printn x)
  (%%cexp ('a -> undefined) "dump_object (%s, 0); fprintf (stdout, \"\\n\")" x))

(define (+ a b)
  (%%cexp (int int -> int) "%s+%s" a b))

(define (other-thing x y z)
  (+ x (+ y z)))

(define (thing a)
  (other-thing a.x a.y a.z))

(printn (thing {a=9 x=8 y=7 z=6}))
(printn (thing {x=3 y=4 z=5}))

