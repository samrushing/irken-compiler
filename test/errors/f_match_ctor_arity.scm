;; -*- Mode: Irken -*-

;; a constructor pattern with the wrong number of fields.

;; expect-error: f_match_ctor_arity.scm:10:3: error: `cons` has 2 fields, but this pattern has 1

(include "lib/basis.scm")

(define f
  (list:cons x) -> x
  (list:nil) -> 0)

(printn (f (list 1)))
