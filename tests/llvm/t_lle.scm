;; -*- Mode: Irken -*-

(define (+ a b)
  (%llarith add a b))

(define (- a b)
  (%llarith sub a b))

(define (1- a)
  (%llarith sub a 1))

(define x 33)

(define (^llvm-x)
  x)

(define (^llvm-a a)
  (+ a 3))

;; (define (^llvm-b b c)
;;   (+ (^llvm-x) 
;;      (1- (^llvm-a (+ b (- c 1))))
;;      ))

(define (^llvm-b)
  (+ (+ (^llvm-x) 
	(^llvm-x))
     (^llvm-x)))

(^llvm-b)
