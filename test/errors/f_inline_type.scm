;; -*- Mode: Irken -*-

;; a type error inside a function that gets inlined: in its body.

;; the type error is either in `g` (C backend: in the body of string-length)
;;  or at the argument 3 (native backends), so only the message is checked.

;; expect-error: error: type error

(include "lib/basis.scm")
(define (g x) (string-length x))
(define (f) (g 3))
(printn (f))
