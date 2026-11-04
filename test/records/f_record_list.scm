;; -*- Mode: Irken -*-

;; records of different shapes are different types, so they can't
;;  share a list.

;; expect-error: Type Error

(include "lib/basis.scm")

(printn (length (list {x=1 y=2} {x=3 z=4})))
