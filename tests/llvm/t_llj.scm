;; -*- Mode: Irken -*-

(define (+ a b)
  (%llarith add a b))

;(define (+ a b)
;  (%%cexp (int int -> int) "%0+%1" a b))

(define (^llvm-x a)
  (define (x b)
    (+ a b))
  x)

((^llvm-x 19) 3)

