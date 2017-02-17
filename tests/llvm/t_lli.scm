;; -*- Mode: Irken -*-

(define (+ a b)
  (%llarith add a b))

;(define (+ a b)
;  (%%cexp (int int -> int) "%0+%1" a b))

(define (^llvm-x a)
  (lambda (b)
    (+ a b))
  )

((^llvm-x 19) 3)

