
;;(include "lib/core.scm")
;;(include "lib/vector.scm")

;; a quick translation of okasaki's pure functional red-black tree

(define (make-tree < root)
  (let ((t (make-vector 2)))
    (vector-set! t 0 <)
    (vector-set! t 1 root)
    t))

(define (tree/< t)
  (vector-ref t 0))
(define (tree/root t)
  (vector-ref t 1))

;; ok, it just about *kills* me to waste 64 bits on <red?>.
;;  possible solutions:
;;  1) when we get user-defined tuple types, use a red and a black tuple type.
;;  2) store 'rank' info in there.

(define (make-node red? left right key val)
  (let ((node (make-vector 5)))
    (vector-set! node 0 red?)
    (vector-set! node 1 left)
    (vector-set! node 2 right)
    (vector-set! node 3 key)
    (vector-set! node 4 val)
    node))

(define (node/red? node)
  (vector-ref node 0))
(define (node/left node)
  (vector-ref node 1))
(define (node/right node)
  (vector-ref node 2))
(define (node/key node)
  (vector-ref node 3))
(define (node/val node)
  (vector-ref node 4))

(define (red-node l r k v)
  (make-node #t l r k v))

(define (black-node l r k v)
  (make-node #f l r k v))

;; my lame attempt at grokking okasaki's ML code.
;; gee, pattern matching would sure be sweet.

(define (balance red? l r k v)
  (cond ((and l (node/red? l) (node/left l) (node/red? (node/left l)))
	 (red-node (black-node (node/left (node/left l)) (node/right (node/left l)) (node/key (node/left l)) (node/val (node/left l)))
		   (black-node (node/right l) r k v)
		   (node/key l)
		   (node/val l)))
	((and l (node/red? l) (node/right l) (node/red? (node/right l)))
	 (red-node (black-node (node/left l) (node/left (node/right l)) (node/key l) (node/val l))
		   (black-node (node/right (node/right l)) r k v)
		   (node/key (node/right l))
		   (node/val (node/right l))))
	((and r (node/red? r) (node/left r) (node/red? (node/left r)))
	 (red-node (black-node l (node/left (node/left r)) k v)
		   (black-node (node/right (node/left r)) (node/right r) (node/key r) (node/val r))
		   (node/key (node/left r))
		   (node/val (node/left r))))
	((and r (node/red? r) (node/right r) (node/red? (node/right r)))
	 (red-node (black-node l (node/left r) k v)
		   (black-node (node/left (node/right r)) (node/right (node/right r)) (node/key (node/right r)) (node/val (node/right r)))
		   (node/key r)
		   (node/val r)))
	(else (make-node red? l r k v))))
	 
(define (tree-insert tree k v)
  (let ((< (tree/< tree))
	(root (tree/root tree)))
    (define (ins node)
      (cond ((%eq? node #f)
	     (make-node #t #f #f k v))
	    ((< k (node/key node))
	     (balance (node/red? node) (ins (node/left node)) (node/right node) (node/key node) (node/val node)))
	    ((< (node/key node) k)
	     (balance (node/red? node) (node/left node) (ins (node/right node)) (node/key node) (node/val node)))
	    (else node)))
    (let ((s (ins root)))
      (make-tree < (black-node (node/left s) (node/right s) (node/key s) (node/val s))))))

(define (tree-member tree k)
  (let ((< (tree/< tree))
	(root (tree/root tree)))
    (let member0 ((n root))
      (cond ((%eq? n #f) #f)
	    ((< k (node/key n))
	     (member0 (node/left n)))
	    ((< (node/key n) k)
	     (member0 (node/right n)))
	    (else (node/val n))))))

(define (tree-inorder t proc)
  (let inorder0 ((n (tree/root t)))
    (cond (n
	   (inorder0 (node/left n))
	   (proc (node/key n) (node/val n))
	   (inorder0 (node/right n))))))

(define (tree-reverse t proc)
  (let reverse0 ((n (tree/root t)))
    (cond (n (reverse0 (node/right n))
	     (proc (node/key n) (node/val n))
	     (reverse0 (node/left n))))))

(define (make-tree-generator tree)
  (make-generator
   (lambda (consumer)
     (tree-inorder tree consumer)
     (let loop ()
       (consumer 'end-of-tree)
       (loop))
     )))
