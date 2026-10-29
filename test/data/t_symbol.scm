;; -*- Mode: Irken -*-

(include "lib/basis.scm")

(define a 'apple)
(printn (list a (string->symbol "apple")))
(printn (symbol->string a))
(printn (eq? a (string->symbol "apple")))
;; => (apple apple)
;; => "apple"
;; => #t

(define kind
  'apple  -> 'fruit
  'carrot -> 'vegetable
  _       -> 'unknown)
(printn (map kind (list 'apple 'carrot 'rock)))
;; => (fruit vegetable unknown)
