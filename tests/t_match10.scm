
(include "lib/core.scm")

(datatype list
  (:nil)
  (:cons 'a (list 'a))
  )

(define (error)
  (%%cexp (-> 'a) "goto Lreturn")
  (%%cexp (-> 'a) "PXLL_UNDEFINED")
  )

(define car
  (x . _) -> x
  _       -> (error)
  )

(define cdr
  (_ . y) -> y
  _       -> (error)
  )

(cdr '(1 2 3))

(define thing
  (1 2 a b) -> a
  (1 2 a 9) -> a
  ()        -> 0
  _         -> -1
  )

(define duncan
  ((1 2) (3 4 5) (6 7)) -> "idaho"
  _                     -> "donuts"
  )

{ a = (car '(1 2 3))
  b = (cdr '(10 11 12))
  c = (thing '(1 2 9 8))
  d = (duncan '((1 2) (3 4 5) (6 7)))
  e = (duncan '((1 2) (3 4 5 9) (6 7)))
 }
