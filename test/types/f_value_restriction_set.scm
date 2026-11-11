;; -*- Mode: Irken -*-

;; the value restriction: a variable that is assigned is not polymorphic.

;; expect-error: error: type error

(include "lib/basis.scm")

(define x (list:nil))
(define (g) (set! x (list:cons "a" (list:nil))))
(g)
(define (f) (match x with (list:cons n _) -> (+ n 1) _ -> 0))
(printn (f))
