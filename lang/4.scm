;; -*- Mode: Irken -*-

(include "lib/core.scm")

(datatype tree
  (:empty)
  (:node 'a (tree 'a) (tree 'a))
  )

(define indent
  0 -> #f
  n -> (begin (print-string "  ") (indent (- n 1)))
  )

(define tree/print
  d (tree:empty)               -> #f
  d (tree:node val left right) -> (begin
				    (indent d)
				    (tree/print (+ d 1) left)
				    (print val)
				    (print-string "\n")
				    (tree/print (+ d 1) right)))

(let ((t (tree:node
	  5
	  (tree:node 7 (tree:empty) (tree:node 12 (tree:empty) (tree:empty)))
	  (tree:node 9 (tree:empty) (tree:empty))
	  )))
  (tree/print 0 t)
  )
