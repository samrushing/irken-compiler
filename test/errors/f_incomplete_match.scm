;; -*- Mode: Irken -*-

;; a match with no case for one of the constructors.

;; expect-error: f_incomplete_match.scm:12:10: error: incomplete match: no case for c

(include "lib/basis.scm")
(datatype t (:a int) (:b) (:c))
(define f
  (t:a n) -> n
  (t:b) -> 0)
(printn (f (t:c)))
