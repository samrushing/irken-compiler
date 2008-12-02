
(include "lib/core.scm")
(include "lib/vector.scm")
(include "lib/pair.scm")
(include "lib/string.scm")
(include "lib/random.scm")

;; a quick translation of okasaki's pure functional red-black tree

(define (make-node red? left item right)
  (let ((node (make-vector 4)))
    (vector-set! node 0 red?)
    (vector-set! node 1 left)
    (vector-set! node 2 item)
    (vector-set! node 3 right)
    node))

(define (node.red? node)
  (vector-ref node 0))
(define (node.left node)
  (vector-ref node 1))
(define (node.item node)
  (vector-ref node 2))
(define (node.right node)
  (vector-ref node 3))

(define (red-node l x r)
  (make-node #t l x r))

(define (black-node l x r)
  (make-node #f l x r))

;; my lame attempt at grokking okasaki's ML code.
;; gee, pattern matching would sure be sweet.

(define (balance red? l x r)
  ;;(%print red?) (%print l) (%print x) (%printn r)
  (cond ((and l (node.red? l) (node.left l) (node.red? (node.left l)))
	 (red-node (black-node (node.left (node.left l)) (node.item (node.left l)) (node.right (node.left l)))
		   (node.item l)
		   (black-node (node.right l) x r)))
	((and l (node.red? l) (node.right l) (node.red? (node.right l)))
	 (red-node (black-node (node.left l) (node.item l) (node.left (node.right l)))
		   (node.item (node.right l))
		   (black-node (node.right (node.right l)) x r)))
	((and r (node.red? r) (node.left r) (node.red? (node.left r)))
	 (red-node (black-node l x (node.left (node.left r)))
		   (node.item (node.left r))
		   (black-node (node.right (node.left r)) (node.item r) (node.right r))))
	((and r (node.red? r) (node.right r) (node.red? (node.right r)))
	 (red-node (black-node l x (node.left r))
		   (node.item r)
		   (black-node (node.left (node.right r)) (node.item (node.right r)) (node.right (node.right r)))))
	(else (make-node red? l x r))))
	 
(define (insert x tree)
  (define (ins node)
    (cond ((%eq? node #f)
	   (make-node #t #f x #f))
	  ((%lt? x (node.item node))
	   (balance (node.red? node) (ins (node.left node)) (node.item node) (node.right node)))
	  ((%lt? (node.item node) x)
	   (balance (node.red? node) (node.left node) (node.item node) (ins (node.right node))))
	  (else node)))
  (let ((s (ins tree)))
    (black-node (node.left s) (node.item s) (node.right s))))

(define (member x t)
  (cond ((%eq? t #f) #f)
	((%lt? x (node.item t))
	 (member x (node.left t)))
	((%lt? (node.item t) x)
	 (member x (node.right t)))
	(else #t)))

(define (inorder t proc)
  (cond (t (inorder (node.left t) proc)
	   (proc (node.item t))
	   (inorder (node.right t) proc)
	   )))

(let loop ((n 100) (t #f))
  (if (%gt? n 0)
      (loop (%- n 1) (insert (random) t))
      (%printn t)))

(let ((t #f)
      (space " "))
  (let loop ((n 20))
    (cond ((%gt? n 0)
	   (set! t (insert n t))
	   (loop (%- n 1)))))
  (%printn (member 9 t))
  (%printn (member 17 t))
  (%printn (member 21 t))
  (inorder t (lambda (x) (print-int x) (print-string space)))
  (%printn #t)
  )
    
(define (make-gen t)

  (define (generator)
    (^call/cc control-state))

  (define (control-state return)
    (define (take-element elem)
      (set! return
	    (^call/cc
	     (lambda (resume-here)
	       (set! control-state resume-here)
	       (return elem)))))
    (inorder t take-element))

  generator
  )

(let ((t #f)
      (space " "))
  (let loop ((n 20))
    (cond ((%gt? n 0)
	   (set! t (insert n t))
	   (loop (%- n 1)))))
  (let ((g (make-gen t)))
    (%printn g)
    (%printn (g))
    (%printn (g))
    (%printn (g))))
  
