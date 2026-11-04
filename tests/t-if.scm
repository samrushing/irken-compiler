;; -*- Mode: Irken -*-

(define (> a b) (%llicmp sgt a b))
(define (< a b) (%llicmp slt a b))
(define (+ a b) (%llarith add a b))

(+ 19
   (if (> 4 3)
       (if (< 8 12)
           (+ 7 3)
           9)
       42)
   )

