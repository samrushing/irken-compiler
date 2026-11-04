;; -*- Mode: Irken -*-

(define (+ a b)
  (%backend arm (%llarith add a b))
  (%backend c (%%cexp (int int -> int) "%0+%1" a b)))

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

;; avoiding litcon

(define (make-one-thing a)
  (thing:one a))

(define (make-two-thing a b)
  (thing:two a b))

(define (make-three-thing a b c)
  (thing:three a b c))

;;(add-thing (make-two-thing 3 4))
;;(add-thing (make-three-thing 3 4 5))
;;(add-thing (thing:zero))
(add-thing (make-one-thing 33))


