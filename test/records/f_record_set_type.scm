;; -*- Mode: Irken -*-

;; set! of a field to a value of another type is a type error.

;; expect-error: Type Error

(include "lib/basis.scm")

(define r {x=1})
(set! r.x "one")
(printn r.x)
