;; -*- Mode: Irken -*-

;; (define (+ a b)
;;   (%%cexp (int int -> int) "%0+%1" a b))

;; (define (- a b)
;;   (%%cexp (int int -> int) "%0-%1" a b))

(define (+ a b)
  (%llarith add a b))

(define (- a b)
  (%llarith sub a b))

(define (^llvm-x a)
  (let ((b (+ a 3))
	(c (+ b 9)))
    (- b c)))

(^llvm-x 19)
