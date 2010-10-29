;; -*- Mode: Irken -*-

(datatype thing
  (:t int int))

(define second
  (thing:t _ x) -> x
  )

(second (thing:t 5 4))
    