;; -*- Mode: Irken -*-

(define (printn x)
  (%%cexp ('a -> undefined) "dump_object (%s, 0); fprintf (stdout, \"\\n\")" x))

(define (thing a)
  (vcase a
    ((:one x y) x)
    ((:two z)   z)
    ((:urk)     0)
    ))

(printn (thing (:one 12 3)))
(printn (thing (:urk)))
(printn (thing (:two 9)))



