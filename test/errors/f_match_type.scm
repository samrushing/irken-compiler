;; -*- Mode: Irken -*-

;; pattern-matching arms of different types: at the arm.

;; expect-error: f_match_type.scm:10:8: error: type error

(include "lib/basis.scm")
(define f
  0 -> "zero"
  n -> 1)
(printn (f 0))
