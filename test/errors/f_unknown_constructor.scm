;; -*- Mode: Irken -*-

;; a constructor the datatype does not have.

;; expect-error: f_unknown_constructor.scm:10:10: error: datatype `t` has no constructor `c` (it has: a, b)

(include "lib/basis.scm")

(datatype t (:a int) (:b))
(printn (t:c 1))
