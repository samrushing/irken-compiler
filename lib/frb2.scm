
;;(include "lib/core.scm")
;;(include "lib/vector.scm")

(define (make-tuple tag len)
  (%%cexp (int int -> ?) "allocate (%s, %s)" tag len))

(define (eq? x y)
  (%%cexp (? ? -> bool) "%s==%s" x y))

(define (not x)
  (eq? x #f))

;; a quick translation of okasaki's pure functional red-black tree

(class node (red?:bool left:node right:node key val))

;; hack to get a quasi-safe empty node object.
(define (make-empty-node)
  (%%cexp (-> node) "allocate (0x20, 5)"))
  )

(define (empty? n:node)
  (eq? n the-empty-node))

(define the-empty-node (make-empty-node))

(define (red-node l r k v)
  (make-node #t l r k v))

(define (black-node l r k v)
  (make-node #f l r k v))

;; my lame attempt at grokking okasaki's ML code.
;; gee, pattern matching would sure be sweet.
	 
(define (balance red? l:node r:node k v)
  (cond ((and (l.not-empty?) l.red? (l.left.not-empty?) l.left.red?)
	 (red-node (black-node l.left.left l.left.right l.left.key l.left.val)
		   (black-node l.right r k v)
		   l.key
		   l.val))
	((and (l.not-empty?) l.red? (l.right.not-empty?) l.right.red?)
	 (red-node (black-node l.left l.right.left l.key l.val)
		   (black-node l.right.right r k v)
		   l.right.key
		   l.right.val))
	((and (r.not-empty?) r.red? (r.left.not-empty?) r.left.red?)
	 (red-node (black-node l r.left.left k v)
		   (black-node r.left.right r.right r.key r.val)
		   r.left.key
		   r.left.val))
	((and (r.not-empty?) r.red? (r.right.not-empty?) r.right.red?)
	 (red-node (black-node l r.left k v)
		   (black-node r.right.left r.right.right r.right.key r.right.val)
		   r.key
		   r.val))
	(else (make-node red? l r k v))))

(class tree (< root:node)

  (define (init self < root)
    (set! self.< <)
    (set! self.root root))

  (define (insert self k v)
    (let ((< self.<)
	  (root self.root))
      (define (ins n)
	(cond ((eq? n the-empty-node)
	       (make-node #t the-empty-node the-empty-node k v))
	      ((< k n.key)
	       (balance n.red? (ins n.left) n.right n.key n.val))
	      ((< n.key k)
	       (balance n.red? n.left (ins n.right) n.key n.val))
	      (else n)))
      (let ((s (ins root)))
	(make-tree < (black-node s.left s.right s.key s.val))
	)))

  (define (member self k)
    (let ((< self.<)
	  (root self.root))
      (let member0 ((n root))
	(cond ((eq? n the-empty-node) #f)
	      ((< k n.key)
	       (member0 n.left))
	      ((< n.key k)
	       (member0 n.right))
	      (else n.val)))
      ))

  (define (inorder self proc)
    (let inorder0 ((n self.root))
      (cond (n
	     (inorder0 n.left)
	     (proc n.key n.val)
	     (inorder0 n.right)))))

  (define (reverse self proc)
    (let reverse0 ((n self.root))
      (cond (n
	     (reverse0 n.right)
	     (proc n.key n.val)
	     (reverse0 n.left)))))
  )

(define (< a b)
  (%%cexp (int int -> bool) "%s+%s" a b))

(define (make-eq-tree)
  (make-tree < the-empty-node)
  )

(make-eq-tree)
