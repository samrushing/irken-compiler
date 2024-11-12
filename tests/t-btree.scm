;; -*- Mode: Irken -*-

(require "lib/core.scm")
;; (require "lib/alist.scm")
;; (require "lib/pair.scm")
;; (require "lib/format.scm")
;; (require "lib/string.scm")

(datatype btree
  (:node (btree 'a) (btree 'a))
  (:leaf 'a)
  )

(defmacro btree/make
  (btree/make (l r)) -> (btree:node (btree/make l) (btree/make r))
  (btree/make x)     -> (btree:leaf x)
  )

;; Note the `88` in t2.
(define t0 (btree/make ((0 ((1 (2 (3 4))) 5)) (((6 7) ((8 (9 10)) 11)) ((12 (((13 14) 15) (16 17))) (18 19))))))
(define t1 (btree/make (((0 ((1 2) 3)) (((4 5) (((6 7) 8) (9 10))) ((11 ((12 13) 14)) ((15 (16 17)) 18)))) 19)))
(define t2 (btree/make (((0 ((1 2) 3)) (((4 5) (((6 7) 8) (9 10))) ((88 ((12 13) 14)) ((15 (16 17)) 18)))) 19)))

(define btree/inorder
  p (btree:leaf x)   -> (p x)
  p (btree:node l r) -> (begin (btree/inorder p l) (btree/inorder p r))
  )

(printn t0)
(printn t1)
(printn t2)

(btree/inorder printn t0)
(btree/inorder printn t1)
(btree/inorder printn t2)

