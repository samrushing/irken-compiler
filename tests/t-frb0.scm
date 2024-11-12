;; -*- Mode: Irken -*-

(require "lib/core.scm")
(require "lib/pair.scm")
(require "lib/frb.scm")

;; test tree/make macro

(define (member root cmp key)
  (let member0 ((n root))
    (match n with
      (tree:empty)
      -> (maybe:no)

      (tree:red l r k v)
      -> (match (cmp key k) with
           (cmp:<) -> (member0 l)
           (cmp:>) -> (member0 r)
           (cmp:=) -> (maybe:yes v))

      (tree:black l r k v)
      -> (match (cmp key k) with
           (cmp:<) -> (member0 l)
           (cmp:>) -> (member0 r)
           (cmp:=) -> (maybe:yes v))
      )))

(let ((t (tree/make int-cmp
	  (1 "time")
	  (2 "flies")
	  (3 "like")
	  (4 "a")
	  (5 "banana")
	  )))
  (printn t)
  (printn (member t int-cmp 5))
  (printn (member t int-cmp 2))
  )
