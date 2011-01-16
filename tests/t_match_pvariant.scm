;; -*- Mode: Irken -*-

(define (printn x)
  (%%cexp ('a -> undefined) "dump_object (%0, 0); fprintf (stdout, \"\\n\")" x))

(define (eq? a b)
  (%%cexp ('a 'a -> bool) "%0==%1" a b))

(define thing
  (:pair x 2) -> x
  (:pair 3 y) -> y
  (:pair x y) -> x
  (:dude z)   -> z
  )

(printn (thing (:pair 9 2)))
(printn (thing (:pair 3 7)))
(printn (thing (:pair 9 9)))
(printn (thing (:dude 7)))
