;; -*- Mode: Irken -*-

(include "lib/core.scm")

(datatype thing
  (:one)
  (:two)
  (:three)
  )

(define test0
  (thing:one)   -> 11
  (thing:two)   -> 22
  (thing:three) -> 33
  _             -> 44
  )

(define test1
  (thing:one)   -> 11
  (thing:two)   -> 22
  _             -> 44
  )

(test0 (thing:one))
(test1 (thing:one))
