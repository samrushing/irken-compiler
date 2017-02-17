;; -*- Mode: Irken -*-

(define (+ a b)
  (%llarith add a b))

(define (- a b)
  (%llarith sub a b))

(define (^llvm-a a)
  (+ a 3))

(define (^llvm-b b c)
  (^llvm-a (+ b (- c 1))))

(^llvm-b 3 4)
