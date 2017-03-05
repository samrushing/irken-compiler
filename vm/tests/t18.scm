;; -*- Mode: Irken -*-

(include "lib/core.scm")

(define thing
  0 -> 1
  2 -> 3
  n -> (+ n 100)
  )

(print (thing 7))
(print (thing 2))
(print (thing 0))

