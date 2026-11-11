;; -*- Mode: Irken -*-

;; an unknown variable inside a macro call (`when`): at the variable.

;; expect-error: f_unbound_in_macro.scm:10:13: error: unknown variable `zork`

(include "lib/basis.scm")
(define (f x)
  (when (> x 0)
    (printn zork))
  x)
(f 1)
