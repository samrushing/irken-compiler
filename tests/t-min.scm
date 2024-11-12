;; -*- Mode: Irken -*-

(require "lib/core.scm")
(require "lib/pair.scm")

(define list-min
  ()        a -> a
  (hd . tl) a -> (list-min tl (min a hd))
  )

(define list-max
  ()        a -> a
  (hd . tl) a -> (list-max tl (max a hd))
  )

(define min-max
  ()        b s -> (:tuple b s)
  (hd . tl) b s -> (min-max tl (max b hd) (min s hd))
  )

(let ((l '(3 1 4 1 5 9 2 6 5)))
  (printn (list-min l 1000))
  (printn (list-max l -1))
  (printn (foldr1 min l))
  (printn (foldr1 max l))
  (printn (min-max l -1 1000))
  )

