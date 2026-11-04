;; -*- Mode: Irken -*-

(define (+ a b) (%llarith add a b))

(datatype thing
  (:zero)
  (:one int)
  (:two int int)
  (:three int int int)
  )

;; testing the ealt sub in nvcase.
(define add-thing
  (thing:zero)        -> 0
  (thing:two a b)     -> (+ a b)
  _                   -> 9
  )

(add-thing (thing:one 33))
