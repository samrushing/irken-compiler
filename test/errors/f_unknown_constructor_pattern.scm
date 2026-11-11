;; -*- Mode: Irken -*-

;; a constructor the datatype does not have, in a pattern.

;; expect-error: f_unknown_constructor_pattern.scm:12:3: error: datatype `t` has no constructor `c` (it has: a, b)

(include "lib/basis.scm")

(datatype t (:a int) (:b))
(define f
  (t:a n) -> n
  (t:c) -> 0)
(printn (f (t:b)))
