;; -*- Mode: Irken -*-

;; let-polymorphism: one function used at several types.

(include "lib/basis.scm")

(define (my-id x) x)
(define (my-compose f g) (lambda (x) (f (g x))))
(define (twice f) (my-compose f f))

(printn (my-id 3))
(printn (my-id "three"))
(printn (my-id 'three))
;; => 3
;; => "three"
;; => three

(printn ((twice (lambda (x) (* x 2))) 5))
(printn ((twice (lambda (s) (string-append s "!"))) "hi"))
;; => 20
;; => "hi!!"

(define (my-map f l)
  (match l with
    ()        -> '()
    (hd . tl) -> (list:cons (f hd) (my-map f tl))))
(printn (my-map int->string (list 1 2 3)))
(printn (my-map string-length (list "a" "bb" "ccc")))
;; => ("1" "2" "3")
;; => (1 2 3)

;; a polymorphic pair-swapping function used at two types in one expression
(define (swap p)
  (match p with
    (:tuple a b) -> (:tuple b a)))
(let (((x y) (swap (:tuple 1 "a")))
      ((z w) (swap (:tuple #\c 'd))))
  (printn x)
  (printn y)
  (printn z)
  (printn w))
;; => "a"
;; => 1
;; => d
;; => #\c
