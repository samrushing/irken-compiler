;; -*- Mode: Irken -*-

;; a type error from an argument, inside the function it is passed to.

;; the type error is either at the `+` in `f` (C backend) or at the
;;  argument "x" (native backends), so only the message is checked.

;; expect-error: error: type error

(include "lib/basis.scm")
(define (f x) (+ x 1))
(printn (f "x"))
