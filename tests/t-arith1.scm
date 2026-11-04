;; -*- Mode: Irken -*-

(define (- a b)
  (%llarith sub a b))

(define (+ a b)
  (%llarith add a b))

(- (+ 19 34) 7)
