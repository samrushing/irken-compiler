;; -*- Mode: Irken -*-

(require "lib/core.scm")

(datatype thing
  (:a)
  (:b)
  (:c)
  (:d int int)
  )

(define add-thing
  (thing:a)     -> 12
  (thing:b)     -> 7
  (thing:c)     -> 0
  (thing:d a b) -> (+ a b)
  )


(printn (add-thing (thing:a)))
(printn (add-thing (thing:b)))
(printn (add-thing (thing:c)))
(printn (add-thing (thing:d 3 4)))


