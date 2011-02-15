;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/set2.scm")
(include "lib/alist2.scm")

(define (transpose g)
  (let ((gt (alist-maker)))
    (g::iterate
     (lambda (k _)
       (gt::add k (set-maker '()))))
    (g::iterate
     (lambda (k vl)
       (for-each
	(lambda (v)
	  (match (gt::get v) with
	    (maybe:no) -> (gt::add v (set-maker (LIST k)))
	    (maybe:yes s) -> (s::add k)))
	(vl::get))))
    gt))

(define (print-graph g)
  (print-string "graph = {\n")
  (g::iterate
   (lambda (k v)
     (print-string "  ")
     (print k)
     (print-string " ")
     (printn (v::get))))
  (print-string "}\n"))

(define (strongly g)
  (let ((s '())
	(visited (set-maker '())))
    (define (visit0 u)
      (visited::add u)
      (match (g::get u) with
	(maybe:no)     -> #u
	(maybe:yes vl) -> (vl::iterate
			   (lambda (v)
			     (if (not (visited::in v))
				 (visit0 v)))))
      (PUSH s u))
    (g::iterate
     (lambda (u v)
       (if (not (visited::in u))
	   (visit0 u))))
    (let ((gt (transpose g))
	  (visited (set-maker '()))
	  (r0 '())
	  (r1 (set-maker '())))
      (define (visit1 u)
	(visited::add u)
	(match (gt::get u) with
	  (maybe:no) -> #u
	  (maybe:yes vl) -> (vl::iterate
			     (lambda (v)
			       (if (not (visited::in v))
				   (visit1 v)))))
	(r1::add u))
      (while
       (not (null? s))
       (let ((u (pop s)))
	 (if (not (visited::in u))
	     (begin
	       (set! r1 (set-maker '()))
	       (visit1 u)
	       (PUSH r0 (r1::get))))))
      r0)))

(define test-g
  '((foo baz)
    (baz bar)
    (bar foo)
    (biff barf)
    (barf snoo snee)
    (snoo biff)
    (snee)
    (top foo biff)))

(define (make-sample)
  (let ((g (alist-maker)))
    (for-each
     (lambda (l)
       (g::add (car l) (set-maker (cdr l))))
     test-g)
    g))

(let ((g (make-sample)))
  (print-graph g)
  (printn (strongly g)))
