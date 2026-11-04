;; -*- Mode: Irken -*-

(define (+ a b) (%llarith add a b))

(datatype thing
  (:zero)
  (:one int)
  (:two int int)
  (:three int int int)
  )

(define add-thing
  (thing:zero)        -> 0
  (thing:one a)       -> a
  (thing:two a b)     -> (+ a b)
  (thing:three a b c) -> (+ a (+ b c))
  )

;; not avoiding litcon

;;(add-thing (thing:two 3 4))
;;(add-thing (thing:three 3 4 5))
;;(add-thing (thing:zero))
(add-thing (thing:one 33))


