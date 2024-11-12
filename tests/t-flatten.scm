;; -*- Mode: Irken -*-

(require "lib/basis.scm")

;; ((0 1 2) (3 4 5) (6 7 8)) => (0 1 2 3 4 5 6 7 8)
(define flatten
  (one)     -> one
  (hd . tl) -> (append hd (flatten tl))
  ()        -> (list:nil)
  )

(flatten '((0 1 2) (3 4 5) (6 7 8)))
