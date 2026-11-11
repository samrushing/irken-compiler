;; -*- Mode: Irken -*-

;; calling a function on a polymorphic variant it does not handle.

;; expect-error: f_unhandled_variant.scm:12:10: error: type error: no field (or variant) `c` here

(include "lib/basis.scm")

(define f
  (:a x) -> x
  (:b y) -> y)
(printn (f (:c 3)))
