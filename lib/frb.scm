
;; For the almighty tallest, a quick translation of okasaki's pure
;;   functional "red-purple" trees.

;; A more 'natural' representation might be:
;; (datatype node (union (empty) (full bool node node ? ?)))
;; where the color is stored as a bool in each node.
;;
;; Instead, we save space by encoding the color into the header of
;;   each node, which leads to some minor code duplication (due to the
;;   lack of real pattern matching).

(datatype tree
  (red    tree tree 'a 'b)
  (purple tree tree 'a 'b)
  (empty)
  )

(define (tree:insert root < k v)

  (define (lbalance l r k v)
    (vcase tree l
      ((:red ll lr lk lv)
       (vcase tree ll
	 ((:red lll llr llk llv)
	  (tree:red (tree:purple lll llr llk llv) (tree:purple lr r k v) lk lv))
	 ((:purple _ _ _ _)
	  (vcase tree lr
	    ((:red lrl lrr lrk lrv)
	     (tree:red (tree:purple ll lrl lk lv) (tree:purple lrr r k v) lrk lrv))
	    ((:purple _ _ _ _)
	     (tree:purple l r k v))
	    ((:empty)
	     (tree:purple l r k v))))
	 ((:empty)
	  (tree:purple l r k v))))
      ((:purple _ _ _ _)
       (tree:purple l r k v))
      ((:empty)
       (tree:purple l r k v))))
  
  (define (rbalance l r k v)
    (vcase tree r
      ((:red rl rr rk rv)
       (vcase tree rr
	 ((:red rrl rrr rrk rrv)
	  (tree:red (tree:purple l rl k v) (tree:purple rrl rrr rrk rrv) rk rv))
	 ((:purple _ _ _ _)
	  (vcase tree rl
	    ((:red rll rlr rlk rlv)
	     (tree:red (tree:purple l rll k v) (tree:purple rlr rr rk rv) rlk rlv))
	    ((:purple _ _ _ _)
	     (tree:purple l r k v))
	    ((:empty)
	     (tree:purple l r k v))))
	 ((:empty)
	  (tree:purple l r k v))))
      ((:purple _ _ _ _)
       (tree:purple l r k v))
      ((:empty)
       (tree:purple l r k v))))

  (define (ins n)
    (vcase tree n
      ((:empty)
       (tree:red (tree:empty) (tree:empty) k v))
      ((:red l r k2 v2)
       (cond ((< k k2)
	      (tree:red (ins l) r k2 v2))
	     ((< k2 k)
	      (tree:red l (ins r) k2 v2))
	     (else n)))
      ((:purple l r k2 v2)
       (cond ((< k k2)
	      (lbalance (ins l) r k2 v2))
	     ((< k2 k)
	      (rbalance l (ins r) k2 v2))
	     (else n)))))

  (let ((s (ins root)))
    (vcase tree s
      ((:purple _ _ _ _) s)
      ((:red l r k v) (tree:purple l r k v))
      ((:empty) s) ;; impossible, should raise something here?
      )))

(define (tree:member root < key)
  (let member0 ((n root))
    (vcase tree n
       ((:empty) (maybe:no))
       ((:red l r k v)
	(cond ((< key k) (member0 l))
	      ((< k key) (member0 r))
	      (else (maybe:yes v))))
       ((:purple l r k v)
	(cond ((< key k) (member0 l))
	      ((< k key) (member0 r))
	      (else (maybe:yes v)))))))

(define (tree:inorder t p)
  (let inorder0 ((n t))
    (vcase tree n
      ((:empty) #f)
      ((:red l r k v)    (inorder0 l) (p k v) (inorder0 r) #f)
      ((:purple l r k v) (inorder0 l) (p k v) (inorder0 r) #f)
      )))

(define (tree:reverse n p)
  (let reverse0 ((n n))
    (vcase tree n
      ((:empty) #f)
      ((:red l r k v)    (reverse0 r) (p k v) (reverse0 l) #f)
      ((:purple l r k v) (reverse0 r) (p k v) (reverse0 l) #f)
      )))


;; the defn of make-generator, call/cc, etc... makes it pretty hard
;;  to pass more than one arg through a continuation.  so instead we'll
;;  use a 'pair' constructor to iterate through the tree...

(define (tree:make-generator tree end-key end-val)
  (make-generator
   (lambda (consumer)
     (tree:inorder tree (lambda (k v) (consumer (:pair k v))))
     (let loop ()
       (consumer (:pair end-key end-val))
       (loop))
     )
   ))
