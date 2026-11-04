;; -*- Mode: Irken -*-

;; record literals, field access and set!.

(include "lib/basis.scm")

(define p {x=3 y=4})
(printn (list p.x p.y))
;; => (3 4)

(set! p.x 30)
(printn (list p.x p.y))
;; => (30 4)

;; fields of different types
(define person {name="ada" age=36 tags=(list 'math 'engines) initial=#\a})
(printn person.name)
(printn person.age)
(printn person.initial)
(printn person.tags)
;; => "ada"
;; => 36
;; => #\a
;; => (math engines)

;; nested records
(define line {from={x=0 y=0} to={x=3 y=4}})
(printn (list line.from.x line.to.y))
(set! line.to.x 6)
(printn line.to.x)
;; => (0 4)
;; => 6

;; a one-field record
(define box {val=99})
(set! box.val (+ box.val 1))
(printn box.val)
;; => 100

;; a record in a list, and in a vector
(define pts (list {x=1 y=2} {x=3 y=4} {x=5 y=6}))
(printn (map (lambda (p) (+ p.x p.y)) pts))
;; => (3 7 11)

(define vec (list->vector pts))
(let ((p vec[1]))
  (set! p.y 40))
(let ((a vec[1])
      (b (nth pts 1)))
  (printn (list a.y b.y)))
;; => (40 40)

;; field order in a literal doesn't matter
(define q {y=2 x=1})
(printn (list q.x q.y))
;; => (1 2)

;; a record field holding a function
(define calc {op=(lambda (a b) (* a b)) arg=7})
(printn (calc.op calc.arg 6))
;; => 42
