;; -*- Mode: Irken -*-

;; row polymorphism: one function used on records of different shapes,
;;  so the field it reads is at a different position in each.

(include "lib/basis.scm")

(define (get-x r) r.x)
(define (sum-ab r) (+ r.a r.b))

(printn (get-x {x=1}))
(printn (get-x {a=0 x=2}))
(printn (get-x {a=0 b=0 x=3}))
(printn (get-x {x=4 z=0}))
(printn (get-x {a=0 b=0 c=0 d=0 x=5 y=0}))
;; => 1
;; => 2
;; => 3
;; => 4
;; => 5

;; the field's type is polymorphic too
(printn (get-x {x="str" y=1}))
(printn (get-x {x=#\c}))
;; => "str"
;; => #\c

(printn (list (sum-ab {a=1 b=2}) (sum-ab {a=10 b=20 c="ignored"}) (sum-ab {z=0 b=200 a=100})))
;; => (3 30 300)

;; set! through a row-polymorphic function
(define (bump! r) (set! r.n (+ r.n 1)))
(define r1 {n=0})
(define r2 {m=#\m n=10 o="o"})
(define r3 {a=0 b=0 c=0 n=100})
(bump! r1) (bump! r2) (bump! r3) (bump! r3)
(printn (list r1.n r2.n r3.n))
(printn r2.m)
(printn r2.o)
(printn r3.c)
;; => (1 11 102)
;; => #\m
;; => "o"
;; => 0

;; map a field accessor over a list of same-shaped records
(printn (map get-x (list {x=1 y=#\a} {x=2 y=#\b})))
;; => (1 2)

;; a function that builds records from its argument's fields
(define (flip r) {x=r.y y=r.x})
(let ((f (flip {x=1 y=2 z=3})))
  (printn (list f.x f.y)))
;; => (2 1)

;; a polymorphic record constructor
(define (wrap v) {val=v count=1})
(let ((w1 (wrap 5))
      (w2 (wrap "five")))
  (printn (list w1.val w2.count))
  (printn w2.val))
;; => (5 1)
;; => "five"
