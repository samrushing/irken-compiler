;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/alist.scm")
(include "lib/stack.scm")

;; sort a collection <l> into lists with matching <p>
;; <p> must return an eq?-compatible object.  returns an alist of stacks.
(define (collect p l)
  (let loop ((acc (alist/make))
	     (l l))
    (match l with
      () -> acc
      (hd . tl)
      -> (let ((key (p hd)))
	   (match (alist/lookup acc key) with
	     (maybe:no) -> (let ((stack (make-stack)))
			     (stack.push hd)
			     (loop (alist:entry key stack acc) tl))
	     (maybe:yes stack) -> (begin (stack.push hd) (loop acc tl)))))))

(datatype test
  (:red)
  (:green)
  (:blue)
  )

(define R (test:red))
(define G (test:green))
(define B (test:blue))

(define test->color
  (test:red) -> 'red
  (test:green) -> 'green
  (test:blue) -> 'blue
  )

(let ((l (LIST R G B B R R G G B)))
  (alist/iterate
   (lambda (k v)
     (printn (v.get)))
   (collect test->color l)))


