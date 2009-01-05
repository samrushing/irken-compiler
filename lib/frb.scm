
;; For the almighty tallest, a quick translation of okasaki's pure
;;   functional "red-purple" trees.

;; A more 'natural' representation might be:
;; (datatype node (union (empty) (full bool node node ? ?)))
;; where the color is stored as a bool in each node.
;;
;; Instead, we save space by encoding the color into the header of
;;   each node, which leads to some minor code duplication (due to the
;;   lack of real pattern matching).  another issue: the fact that I
;;   don't have proper type variables in this syntax means there's no
;;   way to expression that red and purple nodes have the same key and
;;   value types.

(datatype node
  (union
    (empty)
    (red node node ? ?)
    (purple node node ? ?)
    ))

(define (node:lbalance l r k v)
  (typecase node l
    ((red ll lr lk lv)
     (typecase node ll
       ((red lll llr llk llv)
	(node/red (node/purple lll llr llk llv) (node/purple lr r k v) lk lv))
       ((purple _ _ _ _)
	(typecase node lr
           ((red lrl lrr lrk lrv)
	    (node/red (node/purple ll lrl lk lv) (node/purple lrr r k v) lrk lrv))
	   ((purple _ _ _ _)
	    (node/purple l r k v))
	   ((empty)
	    (node/purple l r k v))))
       ((empty)
	(node/purple l r k v))))
    ((purple _ _ _ _)
     (node/purple l r k v))
    ((empty)
     (node/purple l r k v))))

(define (node:rbalance l r k v)
  (typecase node r
    ((red rl rr rk rv)
     (typecase node rr
       ((red rrl rrr rrk rrv)
	(node/red (node/purple l rl k v) (node/purple rrl rrr rrk rrv) rk rv))
       ((purple _ _ _ _)
	(typecase node rl
          ((red rll rlr rlk rlv)
	   (node/red (node/purple l rll k v) (node/purple rlr rr rk rv) rlk rlv))
	  ((purple _ _ _ _)
	   (node/purple l r k v))
	  ((empty)
	   (node/purple l r k v))))
       ((empty)
	(node/purple l r k v))))
    ((purple _ _ _ _)
     (node/purple l r k v))
    ((empty)
     (node/purple l r k v))))

(define (node:insert root < k v)
  (define (ins n)
    (typecase node n
      ((empty)
       (node/red (node/empty) (node/empty) k v))
      ((red l r k2 v2)
       (cond ((< k k2)
	      (node/red (ins l) r k2 v2))
	     ((< k2 k)
	      (node/red l (ins r) k2 v2))
	     (else n)))
      ((purple l r k2 v2)
       (cond ((< k k2)
	      (node:lbalance (ins l) r k2 v2))
	     ((< k2 k)
	      (node:rbalance l (ins r) k2 v2))
	     (else n)))))
  (let ((s (ins root)))
    (typecase node s
      ((purple _ _ _ _) s)
      ((red l r k v) (node/purple l r k v))
      ((empty) s) ;; impossible, should raise something here?
      )))

(define (node:member root < key)
  (let member0 ((n root))
    (typecase node n
       ((empty) (maybe/no))
       ((red l r k v)
	(cond ((< key k) (member0 l))
	      ((< k key) (member0 r))
	      (else (maybe/yes v))))
       ((purple l r k v)
	(cond ((< key k) (member0 l))
	      ((< k key) (member0 r))
	      (else (maybe/yes v)))))))

(define (print-spaces n)
  (let loop ((n n))
    (cond ((> n 0)
	   (print-string "  ")
	   (loop (- n 1))))))

(define (print-item k v d)
  (print-spaces d)
  (print k)
  (print-string ":")
  (print v)
  (print-string "\n"))

(define (node:print n)
  (let p ((n n) (d 0))
    (typecase node n
      ((empty) #u)
      ((red l r k v)    (p l (+ d 1)) (print-item k v d) (p r (+ d 1)))
      ((purple l r k v) (p l (+ d 1)) (print-item k v d) (p r (+ d 1))))
    ))

(define (node:inorder n p)
  (let inorder0 ((n n))
    (typecase node n
      ((empty))
      ((red l r k v)    (inorder0 l) (p k v) (inorder0 r))
      ((purple l r k v) (inorder0 l) (p k v) (inorder0 r))
      )))

(define (node:reverse n p)
  (let reverse0 ((n n))
    (typecase node n
      ((empty))
      ((red l r k v)    (reverse0 r) (p k v) (reverse0 l))
      ((purple l r k v) (reverse0 r) (p k v) (reverse0 l))
      )))

;; the defn of make-generator, call/cc, etc... makes it pretty hard
;;  to pass more than one arg through a continuation.  so instead we'll
;;  use a 'pair' constructor to iterate through the tree...

(define (node:make-generator tree end-key end-val)
  (make-generator
   (lambda (consumer)
     (node:inorder tree (lambda (k v) (consumer (pair k v))))
     (let loop ()
       (consumer (pair end-key end-val))
       (loop))
     )))
