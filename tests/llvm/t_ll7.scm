;; -*- Mode: Irken -*-

(define (+ a b)
  (%llarith add a b))

(define (- a b)
  (%llarith sub a b))

(define (> a b)
  (%llicmp ugt a b))

(define (>= a b)
  (%llicmp uge a b))

(define (< a b)
  (%llicmp ult a b))

(define (<= a b)
  (%llicmp ule a b))

(define (= a b)
  (%llicmp eq a b))

(define (+ a b)
  (%%cexp (int int -> int) "%0+%1" a b))

(define (> a b)
  (%%cexp (int int -> bool) "%0>%1" a b))

(define (^llvm-thing a b)
  ;;(< (- (+ 99 (+ a b)) 1) 150)
  (+ 
   (if (< a b)
       17
       71)
   (if (= a 19)
       27
       37)
   ))

(^llvm-thing 19 34)

