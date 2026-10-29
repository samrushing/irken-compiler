;; -*- Mode: Irken -*-

(include "lib/basis.scm")

(define l (list 3 1 4 1 5 9 2 6))

(printn l)
(printn (list (length l) (first l) (second l) (last l) (nth l 4)))
;; => (3 1 4 1 5 9 2 6)
;; => (8 3 1 6 5)

(printn (reverse l))
(printn (append (list 1 2) (list 3) '() (list 4 5)))
(printn (map (lambda (x) (* x x)) l))
(printn (filter odd? l))
;; => (6 2 9 5 1 4 1 3)
;; => (1 2 3 4 5)
;; => (9 1 16 1 25 81 4 36)
;; => (3 1 1 5 9)

(printn (fold binary+ 0 l))   ;; (+ is a macro, so not first-class)
(printn (foldr list:cons '() l))
(printn (fold list:cons '() l))
;; => 31
;; => (3 1 4 1 5 9 2 6)
;; => (6 2 9 5 1 4 1 3)

(printn (sort < l))
(printn (sort > l))
;; => (1 1 2 3 4 5 6 9)
;; => (9 6 5 4 3 2 1 1)

(printn (list (member? 9 l =) (member? 7 l =)))
(printn (range 5))
(printn (n-of 3 'x))
;; => (#t #f)
;; => (0 1 2 3 4)
;; => (x x x)

(printn (list (take l 3) (drop l 5)))
;; => ((3 1 4) (9 2 6))

(printn (list (null? '()) (null? l) (all? odd? (list 1 3)) (any? even? (list 1 3))))
;; => (#t #f #t #f)

;; match on list structure
(define pairs
  (a b . rest) -> (list:cons (+ a b) (pairs rest))
  (a)          -> (list a)
  ()           -> '())
(printn (pairs l))
(printn (pairs (list 1 2 3)))
;; => (4 5 14 8)
;; => (3 3)

;; nested lists, and lists of strings
(printn (list (list 1 2) '() (list 3)))
(printn (list (list (list 4))))
(printn (list "a" "b"))
;; => ((1 2) () (3))
;; => (((4)))
;; => ("a" "b")
