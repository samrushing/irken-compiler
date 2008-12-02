(include "lib/core.scm")
(include "lib/vector.scm")
(include "lib/random.scm")
(include "lib/pair.scm")
(include "lib/string.scm")

;; based on the code in scheme48/scheme/big/search-tree.scm

;; mangled: yes, the source is somewhat mangled from the original,
;;  the most egregious foul being the rewriting of many of the
;;  <cond> expressions with <if>.  Halfway through porting this,
;;  I finally just broke down and added <cond> the transform.py.

(define (make-node key val parent)
  (let ((node (make-vector 6)))
    (vector-set! node 0 key)
    (vector-set! node 1 val)
    (vector-set! node 2 parent)
    (vector-set! node 3 #t) ; red?
    (vector-set! node 4 #f) ; left
    (vector-set! node 5 #f) ; right
    node
    ))

;; real schemes have a 'defrecord' that creates these automatically...
(define (node.key node)
  (vector-ref node 0))
(define (node.val node)
  (vector-ref node 1))
(define (node.parent node)
  (vector-ref node 2))
(define (node.red? node)
  (vector-ref node 3))
(define (node.left node)
  (vector-ref node 4))
(define (node.right node)
  (vector-ref node 5))

(define (set-node.key! node key)
  (vector-set! node 0 key))
(define (set-node.val! node val)
  (vector-set! node 1 val))
(define (set-node.parent! node parent)
  (vector-set! node 2 parent))
(define (set-node.red?! node red)
  (vector-set! node 3 red))
(define (set-node.left! node left)
  (vector-set! node 4 left))
(define (set-node.right! node right)
  (vector-set! node 5 right))

(define (make-tree)
  (let* ((t (make-vector 2))
	 (nil (make-node #f #f #f)))
    (set-node.red?! nil #f)
    (vector-set! t 0 nil) ; nil
    (vector-set! t 1 #f)  ; root
    t
    ))

(define (tree.nil t)
  (vector-ref t 0))
(define (tree.root t)
  (vector-ref t 1))
(define (set-tree.nil! t v)
  (vector-set! t 0 v))
(define (set-tree.root! t v)
  (vector-set! t 1 v))

(define (real-tree-min tree delete? k)
  (let ((node (tree.root tree)))
    (if node
	(let loop ((node node))
	  (cond ((node.left node)
		 => loop)
		(else
		 (if delete?
		     (delete! tree node))
		 (k (node.key node) (node.val node)))))
	(k #f #f))))

(define (tree-min tree k)
  (real-tree-min tree #f k))

(define (pop-tree-min! tree k)
  (real-tree-min tree #t k))

(define (real-tree-max tree delete? k)
  (let ((node (tree.root tree)))
    (if node
	(let loop ((node node))
	  (cond ((node.right node)
		 => loop)
		(else
		 (if delete?
		     (delete! tree node))
		 (k (node.key node) (node.val node)))))
	(k #f #f))))

(define (tree-max tree k)
  (real-tree-max tree #f k))

(define (pop-tree-max! tree k)
  (real-tree-max tree #t k))

(define (walk-tree tree proc)
  (let recur ((node (tree.root tree)))
    (if node
	(begin
	  (recur (node.left node))
	  (proc (node.key node) (node.val node))
	  (recur (node.right node))))))

(define (tree-lookup tree key k)
  (let loop ((node (tree.root tree))
	     (parent #f)
	     (left? #f))
    (if (not node)
	(k #f parent left?)
	(if (%eq? (node.key node) key)
	    (k node #f #f)
	    (if (%lt? key (node.key node))
		(loop (node.left node) node #t)
		(loop (node.right node) node #f))))))

(define (node.child node left?)
  (if left?
      (node.left node)
      (node.right node)))

(define (set-node.child! node left? child)
  (if left?
      (set-node.left! node child)
      (set-node.right! node child)))

(define (node-black? node)
  (not (and node (node.red? node))))

(define (successor node)
  (let ((right (node.right node)))
    (if right
	(let loop ((node right))
	  (let ((left (node.left node)))
	    (if left
		(loop left)
		node)))
	(let loop ((node node)
		   (parent (node.parent node)))
	  (if (and parent (%eq? node (node.right parent)))
	      (loop parent (node.parent parent))
	      parent)))))

(define (insert! tree parent left? node)
  ;;(%%cexp "fprintf (stderr, \"insert!\\n\")")
  (if (not parent)
      (set-tree.root! tree node)
      (set-node.child! parent left? node))
  (fixup-insertion! node tree))
      
(define (fixup-insertion! node tree)
  ;;(%%cexp "fprintf (stderr, \"fixup-insertion!\\n\")")
  (let loop ((node node))
    (let ((parent (node.parent node)))
      (if (and parent (node.red? parent))
	  (let* ((grand (node.parent parent))
		 (left? (%eq? parent (node.left grand)))
		 (y (node.child grand (not left?))))
	    (if (node-black? y)
		(let* ((node (if (%eq? node (node.child parent (not left?)))
				 (begin (rotate! parent left? tree)
					parent)
				 node))
		       (parent (node.parent node))
		       (grand (node.parent parent)))
		  (set-node.red?! parent #f)
		  (set-node.red?! grand  #t)
		  (rotate! grand (not left?) tree)
		  (loop node))
		(begin
		   (set-node.red?! parent #f)
		   (set-node.red?! y      #f)
		   (set-node.red?! grand  #t)
		   (loop grand)))))))
  (set-node.red?! (tree.root tree) #f))

;        A                                 B
;       / \    =(rotate! A #f tree)=>     / \
;      B   k                             i   A
;     / \      <=(rotate! B #t tree)=       / \
;    i   j                                 j   k

(define (rotate! node left? tree)
  ;;(%%cexp "fprintf (stderr, \"rotate!\\n\")")
  (let* ((y (node.child node (not left?)))
	 (y-left (node.child y left?))
	 (parent (node.parent node)))
    (set-node.child! node (not left?) y-left)
    (if y-left
	(set-node.parent! y-left node))
    (replace! parent y node tree)
    (set-node.child! y left? node)
    (set-node.parent! node y)))

; Replace CHILD (of PARENT) with NEW-CHILD

(define (replace! parent new-child child tree)
  (set-node.parent! new-child parent)
  (if (%eq? child (tree.root tree))
      (set-tree.root! tree new-child)
      (if (%eq? child (node.left parent))
	  (set-node.left! parent new-child)
	  (set-node.right! parent new-child))))

(define (tree-modify! tree key proc)
  (tree-lookup
   tree key
   (lambda (node parent left?)
     (let ((new-value (proc (if node (node.val node) #f))))
       (cond ((and node new-value)
	      (set-node.val! node new-value))
	     (new-value
	      (insert! tree parent left? (make-node key new-value parent)))
	     (node
	      (delete! tree node)))))))

(define (delete! tree node)
  (let* ((y (cond ((or (not (node.left node))
		       (not (node.right node)))
		   node)
		  (else
		   (let ((y (successor node)))
		     (set-node.key! node (node.key y))
		     (set-node.val! node (node.val y))
		     y))))
	 (x (or (node.left y)
		(node.right y)
		(let ((x (tree.nil tree)))
		  (set-node.right! y x)
		  x)))
	 (parent (node.parent y)))
    (replace! parent x y tree)
    (if (not (node.red? y))
	(fixup-delete! x tree))
    (let ((nil (tree.nil tree)))
      (cond ((node.parent nil)
	     => (lambda (p)
		  (if (%eq? (node.right p) nil)
		      (set-node.right! p #f)
		      (set-node.left! p #f))
		  (set-node.parent! (tree.nil tree) #f)))
	    ((%eq? nil (tree.root tree))
	     (set-tree.root! tree #f))))))

(define (fixup-delete! x tree)
  (let loop ((x x))
    (if (or (%eq? x (tree.root tree))
	    (node.red? x))
	(set-node.red?! x #f)
	(let* ((parent (node.parent x))
	       (left? (%eq? x (node.left parent)))
	       (w (node.child parent (not left?)))
	       (w (cond ((node.red? w)
			 (set-node.red?! w #f)
			 (set-node.red?! parent #t)
			 (rotate! parent left? tree)
			 (node.child (node.parent x) (not left?)))
			(else
			 w))))
	  (cond ((and (node-black? (node.left w))
		      (node-black? (node.right w)))
		 (set-node.red?! w #t)
		 (loop (node.parent x)))
		(else
		 (let ((w (cond ((node-black? (node.child w (not left?)))
				 (set-node.red?! (node.child w left?) #f)
				 (set-node.red?! w #t)
				 (rotate! w (not left?) tree)
				 (node.child (node.parent x) (not left?)))
				(else
				 w))))
		   (let ((parent (node.parent x)))
		     (set-node.red?! w (node.red? parent))
		     (set-node.red?! parent #f)
		     (set-node.red?! (node.child w (not left?)) #f)
		     (rotate! parent left? tree)
		     (set-node.red?! (tree.root tree) #f)))))))))

(define (tree-set! tree key val)
  (tree-modify! tree key (lambda (ignore) val)))

(define (tree-get tree key)
  (tree-lookup
   tree key
   (lambda (node parent left?)
     (if node
	 (node.val node)
	 #f))))

(define (print-node k v)
  (%print (cons k v)))

(define (random-tree size)
  (let ((t (make-tree)))
    (let loop ((n size) (r (random)))
      (cond ((%gt? n 0)
	     (tree-set! t r 0)
	     (loop (%- n 1) (random)))))
    t))

(let ((t0 (make-tree)))
  (tree-set! t0 19 "nineteen")
  (tree-set! t0 12 "twelve")
  (tree-set! t0 0 "zero")
  (%printn (tree-get t0 19))
  (%printn (tree-get t0 0))
  (%printn (tree-get t0 15))
  )
  
(let ((t0 (random-tree 1000)))
  (let loop ((n 1000))
    (cond ((%gt? n 0)
	   (pop-tree-min! t0 print-node)
	   (loop (%- n 1))))))

;; NOTE: don't try to print or return <t>. it contains recursive data structures!
