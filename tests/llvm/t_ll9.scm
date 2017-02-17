;; -*- Mode: Irken -*-

;; (define (+ a b)
;;   (%%cexp (int int -> int) "%0+%1" a b))

;; (define (- a b)
;;   (%%cexp (int int -> int) "%0-%1" a b))

(define (+ a b)
  (%llarith add a b))

(define (- a b)
  (%llarith sub a b))

(define (= a b)
  (%llicmp eq a b))

(define (^llvm-x a)
  (if (= a 0)
      #\X
      (^llvm-x (- a 1))))

(^llvm-x 100)
