;; -*- Mode: Irken -*-

;; two polymorphic-variant patterns whose sub-patterns are of different
;;  datatypes (#3): an error, at the second.

;; expect-error: f_match_pvariant_subtypes.scm:12:3: error: this pattern is a `maybe`, but the first one is a `list`

(include "lib/basis.scm")

(define f
  (:thing x (list:nil)) -> x
  (:thing x (maybe:no)) -> x
  _ -> 0)
(printn (f (:thing 1 (list:nil))))
