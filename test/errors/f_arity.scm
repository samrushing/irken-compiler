;; -*- Mode: Irken -*-

;; calling a function with the wrong number of arguments.

;; expect-error: f_arity.scm:9:10: error: `f` takes 2 arguments, but is called wit

(include "lib/basis.scm")
(define (f a b) (+ a b))
(printn (f 1))
