;; -*- Mode: Irken -*-

(define (= a b)
  (%llicmp eq a b))

(define (+ a b)
  (%llarith add a b))

(define (- a b)
  (%llarith sub a b))

(define (^llvm-mv)
  (let ((v (%make-vector #f 10 0)))
    (for-range i 10
       (set! v[i] (- 10 i)))
    v))

(^llvm-mv)

