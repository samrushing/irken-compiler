;; -*- Mode: Irken -*-

(define (+ a b) (%llarith add a b))
(lambda (x) (set! x (+ x 1)))
