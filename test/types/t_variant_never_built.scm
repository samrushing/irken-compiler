;; -*- Mode: Irken -*-

;; a match arm (or exception handler) for a polymorphic variant that no
;;  code ever constructs can never match: it's dropped, not an error.
;;  (the backends used to stop with "variant constructor never called".)

(include "lib/basis.scm")

(printn (try 1 except (:NeverRaised) -> 2))

(define f
  (:a x) -> x
  (:zz y) -> y
  _ -> 0)
(printn (f (:a 5)))
;; => 1
;; => 5
