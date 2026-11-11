;; -*- Mode: Irken -*-

;; patterns from two different datatypes in one match.

;; expect-error: f_match_datatypes.scm:12:5: error: this pattern is a `list`, but the first one is a `maybe`

(include "lib/basis.scm")

(define (f x)
  (match x with
    (maybe:yes n) -> n
    (list:nil) -> 0))

(printn (f (maybe:yes 1)))
