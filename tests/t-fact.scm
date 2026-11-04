;; -*- Mode: Irken -*-

(define (- a b) (%llarith sub a b))
(define (* a b) (%llarith mul a b))
(define (= a b) (%llicmp eq a b))
(define (eq? a b) (%llicmp eq a b))

(define (fact n a)
  (if (= n 1)
      a
      (fact (- n 1) (* n a))))

(fact 5 1)

