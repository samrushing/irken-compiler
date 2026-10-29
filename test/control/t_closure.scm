;; -*- Mode: Irken -*-

(include "lib/basis.scm")

(define (make-adder n) (lambda (x) (+ x n)))
(define add5 (make-adder 5))
(define add10 (make-adder 10))
(printn (list (add5 1) (add10 1)))
;; => (6 11)

;; each closure captures its own binding
(define fns (map (lambda (i) (lambda () (* i 100))) (range 4)))
(printn (map (lambda (f) (f)) fns))
;; => (0 100 200 300)

;; closures sharing one mutable variable
(define (make-account balance)
  (:tuple (lambda (amt) (set! balance (+ balance amt)) balance)
          (lambda (amt) (set! balance (- balance amt)) balance)))
(let (((deposit withdraw) (make-account 100)))
  (deposit 50)
  (withdraw 30)
  (printn (deposit 0)))
;; => 120

;; deeply nested closures reaching outward
(define (outer a)
  (lambda (b)
    (lambda (c)
      (lambda (d)
        (list a b c d)))))
(printn ((((outer 1) 2) 3) 4))
;; => (1 2 3 4)

;; a function that returns a function that returns itself
(define (self-ref n)
  (if (= n 0)
      0
      (+ 1 (self-ref (- n 1)))))
(printn (self-ref 1000))
;; => 1000
