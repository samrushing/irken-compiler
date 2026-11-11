;; -*- Mode: Irken -*-

;; a type error in a macro expansion (`when`): at the macro call.

;; expect-error: f_macro_type.scm:9:3: error: type error

(include "lib/basis.scm")
(define (f x)
  (when x (printn 1)))
(f 3)
