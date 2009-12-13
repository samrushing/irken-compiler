
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


(define (tree:insert root < k v)

  (define (lbalance l r k v)
    (vcase l
      ((:red ll lr lk lv)
       (vcase ll
         ((:red lll llr llk llv)
  	(:red (:purple lll llr llk llv) (:purple lr r k v) lk lv))
         ((:purple _ _ _ _)
  	(vcase lr
  	  ((:red lrl lrr lrk lrv)
  	   (:red (:purple ll lrl lk lv) (:purple lrr r k v) lrk lrv))
  	  ((:purple _ _ _ _)
  	   (:purple l r k v))
  	  ((:empty)
  	   (:purple l r k v))))
         ((:empty)
  	(:purple l r k v))))
      ((:purple _ _ _ _)
       (:purple l r k v))
      ((:empty)
       (:purple l r k v))))
  
  (define (rbalance l r k v)
    (vcase r
      ((:red rl rr rk rv)
       (vcase rr
         ((:red rrl rrr rrk rrv)
  	(:red (:purple l rl k v) (:purple rrl rrr rrk rrv) rk rv))
         ((:purple _ _ _ _)
  	(vcase rl
            ((:red rll rlr rlk rlv)
  	   (:red (:purple l rll k v) (:purple rlr rr rk rv) rlk rlv))
  	  ((:purple _ _ _ _)
  	   (:purple l r k v))
  	  ((:empty)
  	   (:purple l r k v))))
         ((:empty)
  	(:purple l r k v))))
      ((:purple _ _ _ _)
       (:purple l r k v))
      ((:empty)
       (:purple l r k v))))

  (define (ins n)
    (vcase n
      ((:empty)
       (:red (:empty) (:empty) k v))
      ((:red l r k2 v2)
       (cond ((< k k2)
	      (:red (ins l) r k2 v2))
	     ((< k2 k)
	      (:red l (ins r) k2 v2))
	     (else n)))
      ((:purple l r k2 v2)
       (cond ((< k k2)
	      (lbalance (ins l) r k2 v2))
	     ((< k2 k)
	      (rbalance l (ins r) k2 v2))
	     (else n)))))

  (let ((s (ins root)))
    (vcase s
      ((:purple _ _ _ _) s)
      ((:red l r k v) (:purple l r k v))
      ((:empty) s) ;; impossible, should raise something here?
      )))

(define (tree:member root < key)
  (let member0 ((n root))
    (vcase n
       ((:empty) (:no))
       ((:red l r k v)
	(cond ((< key k) (member0 l))
	      ((< k key) (member0 r))
	      (else (:yes v))))
       ((:purple l r k v)
	(cond ((< key k) (member0 l))
	      ((< k key) (member0 r))
	      (else (:yes v)))))))

(define (tree:inorder t p)
  (let inorder0 ((n t))
    (vcase n
      ((:empty) #f)
      ((:red l r k v)    (inorder0 l) (p k v) (inorder0 r) #f)
      ((:purple l r k v) (inorder0 l) (p k v) (inorder0 r) #f)
      )))

(define (tree:reverse n p)
  (let reverse0 ((n n))
    (vcase n
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
