;; -*- Mode: Irken -*-

(include "lib/core.scm")

(datatype list
  (:nil)
  (:cons 'a (list 'a))
  )

(define range
  0 acc -> acc
  n acc -> (range (- n 1) (list:cons n acc))
  )

(range 5 (list:nil))

