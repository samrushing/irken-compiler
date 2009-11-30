
(define (printn x)
  (%%cexp ('a -> undefined) "dump_object (%s, 0); fprintf (stdout, \"\\n\")" x))

(define (+ a b)
  (%%cexp (int int -> int) "%s+%s" a b))

(define (thing a)
  (other-thing a.x a.y a.z))

(define (other-thing x y z)
  (+ x (+ y z)))

;(define (make0)
;  {a=9 x=3 y=4 z=5 q=9})

;(define (make1)
;  {x=3 y=4 z=5})

;(thing (make0))
;(thing (make1))

(printn (thing {a=9 x=8 y=7 z=6}))
(printn (thing {x=3 y=4 z=5}))

