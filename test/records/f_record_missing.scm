;; -*- Mode: Irken -*-

;; reading a field the record doesn't have is a type error.

;; expect-error: Type Error

(include "lib/basis.scm")

(define (get-x r) r.x)
(printn (get-x {y=1}))
