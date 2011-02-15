;; -*- Mode: Irken -*-

(define (eq? a b)
  (%%cexp ('a 'a -> bool) "%0==%1" a b))

(define thing
  0 2 1 -> 1
  1 3 2 -> 0
  _ _ _ -> 2
  )

(thing 3 2 2)
