;; -*- Mode: Irken -*-

(require "lib/core.scm")

(datatype list
  (:nil)
  (:cons 'a (list 'a))
  )

(defmacro pop!
  (pop! l) -> (match l with
	       (list:nil) -> (error "pop!")
	       (list:cons hd tl) -> (begin (set! l tl) hd)))

(define length*
  ()       acc -> acc
  (_ . tl) acc -> (length* tl (+ 1 acc)))

(define (length l)
  (length* l 0))

(define thing '(3 1 4 1 5 9 2 6 5))

(printn (length thing))

(let ((x (pop! thing)))
  (printn thing))


