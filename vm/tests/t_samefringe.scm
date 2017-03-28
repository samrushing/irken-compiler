;; -*- Mode: Irken -*-

(include "lib/core.scm")

(datatype btree
  (:node (btree 'a) (btree 'a))
  (:leaf 'a))

(defmacro btree/make
  (btree/make (l r)) -> (btree:node (btree/make l) (btree/make r))
  (btree/make x)     -> (btree:leaf x))

(define t0 (literal (btree/make ((0 ((1 (2 (3 4))) 5)) (((6 7) ((8 (9 10)) 11)) ((12 (((13 14) 15) (16 17))) (18 19)))))))
(define t1 (literal (btree/make (((0 ((1 2) 3)) (((4 5) (((6 7) 8) (9 10))) ((11 ((12 13) 14)) ((15 (16 17)) 18)))) 19))))
(define t2 (literal (btree/make (((0 ((1 2) 3)) (((4 5) (((6 7) 8) (9 10))) ((88 ((12 13) 14)) ((15 (16 17)) 18)))) 19))))

(define btree/inorder
  p (btree:leaf x)   -> (begin (p x) #u)
  p (btree:node l r) -> (begin (btree/inorder p l) (btree/inorder p r) #u))

(define (btree/make-generator t)
  (make-generator
   (lambda (consumer)
     (btree/inorder
      (lambda (x) (consumer (maybe:yes x)))
      t)
     (forever (consumer (maybe:no))))))

(define (same-fringe t0 t1 =)
  (let ((g0 (btree/make-generator t0))
	(g1 (btree/make-generator t1)))
    (let loop ((m0 (g0)) (m1 (g1)))
      (match m0 m1 with
	(maybe:yes v0) (maybe:yes v1)
	-> (if (= v0 v1)
	       (loop (g0) (g1))
	       (print "NOT equal"))
	(maybe:no) (maybe:no)
	-> (print "equal")
	_ _ -> (print "unequal size")))))

(same-fringe t0 t1 =)
(same-fringe t0 t2 =)
