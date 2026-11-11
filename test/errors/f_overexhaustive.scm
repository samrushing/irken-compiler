;; -*- Mode: Irken -*-

;; a catch-all case after patterns that already cover every constructor.

;; expect-error: f_overexhaustive.scm:13:20: error: this case can never be reached

(include "lib/basis.scm")

(datatype thing (:one) (:two))
(define f
  (thing:one)   -> 11
  (thing:two)   -> 22
  _             -> 44
  )
(printn (f (thing:one)))
