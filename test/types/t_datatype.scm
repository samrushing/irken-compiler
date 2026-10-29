;; -*- Mode: Irken -*-

(include "lib/basis.scm")

(datatype shape
  (:circle int)
  (:rect int int)
  (:point))

(define area
  (shape:circle r) -> (* 3 (* r r))
  (shape:rect w h) -> (* w h)
  (shape:point)    -> 0)

(printn (map area (list (shape:circle 2) (shape:rect 3 4) (shape:point))))
;; => (12 12 0)

;; a recursive, polymorphic datatype
(datatype btree
  (:leaf)
  (:node (btree 'a) 'a (btree 'a)))

(define (btree/insert t x)
  (match t with
    (btree:leaf) -> (btree:node (btree:leaf) x (btree:leaf))
    (btree:node l v r)
    -> (if (< x v)
           (btree:node (btree/insert l x) v r)
           (btree:node l v (btree/insert r x)))))

(define btree/inorder
  (btree:leaf)       -> '()
  (btree:node l v r) -> (append (btree/inorder l) (list v) (btree/inorder r)))

(define btree/depth
  (btree:leaf)       -> 0
  (btree:node l _ r) -> (+ 1 (max (btree/depth l) (btree/depth r))))

(let ((t (fold (lambda (x t) (btree/insert t x)) (btree:leaf) (list 5 2 8 1 9 3))))
  (printn (btree/inorder t))
  (printn (btree/depth t)))
;; => (1 2 3 5 8 9)
;; => 3

;; nested patterns, and a literal in a pattern
(datatype expr
  (:num int)
  (:add expr expr)
  (:mul expr expr))

(define simplify
  (expr:add (expr:num 0) e) -> (simplify e)
  (expr:add e (expr:num 0)) -> (simplify e)
  (expr:mul (expr:num 1) e) -> (simplify e)
  (expr:add a b)            -> (expr:add (simplify a) (simplify b))
  (expr:mul a b)            -> (expr:mul (simplify a) (simplify b))
  e                         -> e)

(define eval-expr
  (expr:num n)   -> n
  (expr:add a b) -> (+ (eval-expr a) (eval-expr b))
  (expr:mul a b) -> (* (eval-expr a) (eval-expr b)))

(define show
  (expr:num n)   -> (int->string n)
  (expr:add a b) -> (format "(" (show a) "+" (show b) ")")
  (expr:mul a b) -> (format (show a) "*" (show b)))

(let ((e (expr:add (expr:num 0) (expr:mul (expr:num 1) (expr:add (expr:num 2) (expr:num 3))))))
  (printn (show e))
  (printn (show (simplify e)))
  (printn (eval-expr e)))
;; => "(0+1*(2+3))"
;; => "(2+3)"
;; => 5

;; maybe
(define (safe-div a b)
  (if (= b 0) (maybe:no) (maybe:yes (/ a b))))
(define show-maybe
  (maybe:yes x) -> (format "yes " (int x))
  (maybe:no)    -> "no")
(printn (list (show-maybe (safe-div 10 2)) (show-maybe (safe-div 1 0))))
;; => ("yes 5" "no")

