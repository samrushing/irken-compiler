;; -*- Mode: Irken -*-

(define (> a b) (%llicmp sgt a b))
(define (< a b) (%llicmp slt a b))
(define (+ a b) (%llarith add a b))

(> 4 3)
