;; -*- Mode: Irken -*-

(define (- a b) (%llarith sub a b))
(define (+ a b) (%llarith add a b))
(define (* a b) (%llarith mul a b))
(define (= a b) (%llicmp eq a b))

(let ((x (* 34 19))
      (y (+ 7 12)))
  (:thing (- x y))
  )


