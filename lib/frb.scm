
;; For the almighty tallest, a quick translation of okasaki's pure
;;   functional "red-purple" trees.

;; A more 'natural' representation might be:
;; (datatype node (union (empty) (full bool node node ? ?)))
;; where the color is stored as a bool in each node.
;;
;; Instead, we save space by encoding the color into the header of
;;   each node.

(datatype tree
  (:red    (tree 'a 'b) (tree 'a 'b) 'a 'b)
  (:purple (tree 'a 'b) (tree 'a 'b) 'a 'b)
  (:empty)
  )

(define (tree:insert root < k v)

  ;; you can't have a red node directly underneath another red node.
  ;; these two functions detect that condition and adjust the tree to
  ;; maintain that invariant.

  (define lbalance
    (tree:red (tree:red A B k0 v0) C k1 v1) D k2 v2 -> (tree:red (tree:purple A B k0 v0) (tree:purple C D k2 v2) k1 v1)
    (tree:red A (tree:red B C k1 v1) k0 v0) D k2 v2 -> (tree:red (tree:purple A B k0 v0) (tree:purple C D k2 v2) k1 v1)
                                            A B k v -> (tree:purple A B k v))
  
  (define rbalance
    A (tree:red (tree:red B C k1 v1) D k2 v2) k0 v0 -> (tree:red (tree:purple A B k0 v0) (tree:purple C D k2 v2) k1 v1)
    A (tree:red B (tree:red C D k2 v2) k1 v1) k0 v0 -> (tree:red (tree:purple A B k0 v0) (tree:purple C D k2 v2) k1 v1)
                                            A B k v -> (tree:purple A B k v))

  (define (ins n)
    (match n with
       (tree:empty)
       -> (tree:red (tree:empty) (tree:empty) k v)

       (tree:red l r k2 v2)
       -> (cond ((< k k2)
		 (tree:red (ins l) r k2 v2))
		((< k2 k)
		 (tree:red l (ins r) k2 v2))
		(else n))
       
       (tree:purple l r k2 v2)
       -> (cond ((< k k2)
	      (lbalance (ins l) r k2 v2))
	     ((< k2 k)
	      (rbalance l (ins r) k2 v2))
	     (else n))))

  (let ((s (ins root)))
    (match s with
      (tree:red l r k0 v0) -> (tree:purple l r k0 v0)
      _ -> s
      ))

  )

(define (tree:member root < key)
  (let member0 ((n root))
    (match n with
      (tree:empty)
      -> (maybe:no)

      (tree:red l r k v)
      -> (cond ((< key k) (member0 l))
	       ((< k key) (member0 r))
	       (else (maybe:yes v)))

      (tree:purple l r k v)
      -> (cond ((< key k) (member0 l))
	       ((< k key) (member0 r))
	       (else (maybe:yes v)))
      )))

(define tree:inorder
  _ (tree:empty) -> #f
  p (tree:red l r k v)    -> (begin (tree:inorder p l) (p k v) (tree:inorder p r) #f)
  p (tree:purple l r k v) -> (begin (tree:inorder p l) (p k v) (tree:inorder p r) #f)
  )

(define tree:reverse
  _ (tree:empty) -> #f
  p (tree:red l r k v)    -> (begin (tree:reverse p r) (p k v) (tree:reverse p l) #f)
  p (tree:purple l r k v) -> (begin (tree:reverse p r) (p k v) (tree:reverse p l) #f)
  )

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
