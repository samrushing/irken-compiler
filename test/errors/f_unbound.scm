;; -*- Mode: Irken -*-

;; an unknown variable.

;; expect-error: f_unbound.scm:8:20: error: unknown variable `y`

(include "lib/basis.scm")
(define (f x) (+ x y))
(printn (f 1))
