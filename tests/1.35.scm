
(include "lib/core.scm")
(include "lib/pair.scm")

(define (leaf bintree) bintree)

(define (interior-node s rtree ltree)
  (cons s (cons rtree (cons ltree '()))))

(define (leaf? bintree)
  (integer? bintree))

(define (left-child bintree)
  (cadr bintree))

(define (right-child bintree)
  (caddr bintree))

(define (contents-of bintree)
  (car bintree))

(define (number-leaves tree)
  (define (number t c k)
    (if (leaf? t)
	(k c (%+ c 1))
	;; compute left sub-tree
	(number
	 (left-child t) c
	 (lambda (lt c)
	   ;; then the right sub-tree
	   (number
	    (right-child t) c
	    (lambda (rt c)
	      ;; then build a new node and feed it to the continuation
	      (k (interior-node (contents-of t) lt rt) c)))))))
  (number tree 0 (lambda (r c) r)))

(let ((tree (interior-node "foo"
			   (interior-node "bar"
					  (leaf 26)
					  (leaf 12))
			   (interior-node "baz"
					  (leaf 11)
					  (interior-node "quux"
							 (leaf 117)
							 (leaf 14))))))
  (%printn (integer? 34))
  (%printn (leaf? 26))
  (%printn (number-leaves tree)))
	 