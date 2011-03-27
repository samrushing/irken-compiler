;; -*- Mode: Irken -*-

;; http://eternallyconfuzzled.com/tuts/datastructures/jsw_tut_andersson.aspx
;; http://en.wikipedia.org/wiki/AA_tree

;; this code is based on the C examples given by Julienne, and as such isn't
;;   very representative of an Irken/ML style.  I'd like to make a pure functional
;;   implementation, and if possible make skew & split tail-recursive by making
;;   them work from the bottom up rather than top down.  [another approach would be
;;   to just adapt Julienne's non-recursive code].

(define tree/nil
  {level = 0
   left  = tree/nil
   right = tree/nil
   key = (magic #\A)
   val = (magic #\A)
   })

;; shame that doesn't just work.
(set! tree/nil.left tree/nil)
(set! tree/nil.right tree/nil)

(define (node/make level key val left right)
  { level = level
    key   = key
    val   = val
    left  = left
    right = right })

(define (tree/empty) tree/nil)

(define (tree/skew d)
  (if (and (> d.level 0)
	   (= d.left.level d.level))
      (node/make
       d.level d.left.key d.left.val d.left.left 
       (node/make
	d.level d.key d.val d.left.right
	(tree/skew d.right)))
      d))

(define (tree/split b)
  (if (and (= b.right.right.level b.level)
	   (not (= b.level 0)))
      (node/make
       (+ 1 b.level) b.right.key b.right.val
       (node/make b.level b.key b.val b.left b.right.left)
       (tree/split b.right.right))
      b))
       
(define (tree/insert root < key val)
  (let loop ((n root))
    (if (= n.level 0)
	(node/make 1 key val tree/nil tree/nil)
	(tree/split
	 (tree/skew
	  (if (< key n.key)
	      (node/make n.level n.key n.val (loop n.left) n.right)
	      (node/make n.level n.key n.val n.left (loop n.right)))
	  )))))

;; XXX make this pure.

(define (tree/delete root key key-less? key-equal?)
  (let recur ((root root) (key key))
    (if (not (eq? root tree/nil))
	(if (key-equal? key root.key)
	    (if (and (not (eq? root.left tree/nil))
		     (not (eq? root.right tree/nil)))
		(let loop ((heir root.left))
		  (cond ((not (eq? heir.right tree/nil))
			 (loop heir.right))
			(else
			 (set! root.key heir.key)
			 (set! root.val heir.val)
			 (set! root.left (recur root.left root.key)))))
		(set! root (if (eq? root.left tree/nil) root.right root.left)))
	    (if (key-less? root.key key)
		(set! root.right (recur root.right key))
		(set! root.left (recur root.left key))
		)))
    (if (or (< root.left.level (- root.level 1))
	    (< root.right.level (- root.level 1)))
	(begin
	  (set! root.level (- root.level 1))
	  (if (> root.right.level root.level)
	      (set! root.right.level root.level))
	  (tree/skew (tree/split root)))
	root
	)))

(define (tree/member root < key)
  (let member0 ((t root))
    (cond ((= t.level 0) (maybe:no))
	  ((< key t.key) (member0 t.left))
	  ((< t.key key) (member0 t.right))
	  (else (maybe:yes t.val)))))

(define (tree/inorder p t)
  (let recur ((t t))
    (cond ((= t.level 0) #u)
	  (else
	   (recur t.left)
	   (p t.key t.val)
	   (recur t.right)))))

(define (tree/reverse p t)
  (let recur ((t t))
    (cond ((= t.level 0) #u)
	  (else
	   (recur t.right)
	   (p t.key t.val)
	   (recur t.left)))))

(define (tree/keys t)
  (let ((r '()))
    (tree/reverse (lambda (k v) (PUSH r k)) t)
    r))

(define (tree/values t)
  (let ((r '()))
    (tree/reverse (lambda (k v) (PUSH r v)) t)
    r))

(define (tree/dump d p t)
  (let recur ((d d) (t t))
    (if (= t.level 0)
	#u
	(begin
	  (recur (+ d 1) t.left)
	  (p t.key t.val d)
	  (recur (+ d 1) t.right)))))

;; the defn of make-generator, call/cc, etc... makes it pretty hard
;;  to pass more than one arg through a continuation.  so instead we'll
;;  use a 'pair' constructor to iterate through the tree...

(define (tree/make-generator tree)
  (make-generator
   (lambda (consumer)
     (tree/inorder
      (lambda (k v)
	(consumer (maybe:yes (:pair k v))))
      tree)
     (forever (consumer (maybe:no))))
   ))
