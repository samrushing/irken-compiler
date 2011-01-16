
(define (eq? a b)
  (%%cexp ('a 'a -> bool) "%0==%1" a b))

(define (+ a b)
  (%%cexp (int int -> int) "%0+%1" a b))

(define thing
  0 1 -> 2
  0 y -> 3
  1 0 -> 4
  1 x -> (+ x 5)
  y z -> (+ y z)
  )

(thing 10 20)
