;; -*- Mode: Irken -*-

(include "lib/core.scm")

(%backend c

(define (binary+ a b)
  (%%cexp (int int -> int) "%0+%1" a b))

)

(%backend llvm

(define (binary+ a b)
  (%llarith add a b))

)

(defmacro +
  (+ x)       -> x
  (+ a b ...) -> (binary+ a (+ b ...)))

(+ 1 2 3 4 5)
