;; -*- Mode: Irken -*-

;; A translation of Okasaki's red-black trees.

;; [In honor of the almighty tallest, these should be red/purple trees.]

(datatype tree
  (:red   (tree 'a 'b) (tree 'a 'b) 'a 'b)
  (:black (tree 'a 'b) (tree 'a 'b) 'a 'b)
  (:empty)
  )

(define (tree/empty) (tree:empty))

;; you can't have a red node directly underneath another red node.
;; these two functions detect that condition and adjust the tree to
;; maintain that invariant.

(define lbalance
  (tree:red (tree:red A B k0 v0) C k1 v1) D k2 v2 -> (tree:red (tree:black A B k0 v0) (tree:black C D k2 v2) k1 v1)
  (tree:red A (tree:red B C k1 v1) k0 v0) D k2 v2 -> (tree:red (tree:black A B k0 v0) (tree:black C D k2 v2) k1 v1)
                                          A B k v -> (tree:black A B k v))
(define rbalance
  A (tree:red (tree:red B C k1 v1) D k2 v2) k0 v0 -> (tree:red (tree:black A B k0 v0) (tree:black C D k2 v2) k1 v1)
  A (tree:red B (tree:red C D k2 v2) k1 v1) k0 v0 -> (tree:red (tree:black A B k0 v0) (tree:black C D k2 v2) k1 v1)
                                          A B k v -> (tree:black A B k v))

(define (tree/insert root cmp k v)

  (define (ins n)
    (match n with
      (tree:empty)
      -> (tree:red (tree:empty) (tree:empty) k v)

      (tree:red l r k2 v2)
      -> (match (cmp k k2) with
           (cmp:<) -> (tree:red (ins l) r k2 v2)
           (cmp:>) -> (tree:red l (ins r) k2 v2)
           (cmp:=) -> n
           )
      (tree:black l r k2 v2)
      -> (match (cmp k k2) with
           (cmp:<) -> (lbalance (ins l) r k2 v2)
           (cmp:>) -> (rbalance l (ins r) k2 v2)
           (cmp:=) -> n
           )))

  (let ((s (ins root)))
    (match s with
      (tree:red l r k0 v0) -> (tree:black l r k0 v0)
      _ -> s
      ))

  )

;; deletion translated from https://github.com/bmeurer/ocaml-rbtrees

(define (tree/delete-check root cmp k)

  (define lunbalanced
    (tree:red (tree:black A B k0 v0) C k1 v1)
    -> (:tuple (lbalance (tree:red A B k0 v0) C k1 v1) #f)
    (tree:black (tree:black A B k0 v0) C k1 v1)
    -> (:tuple (lbalance (tree:red A B k0 v0) C k1 v1) #t)
    (tree:black (tree:red A (tree:black B C k1 v1) k0 v0) D k2 v2)
    -> (:tuple (tree:black A (lbalance (tree:red B C k1 v1) D k2 v2) k0 v0) #f)
    _ -> (impossible)
    )

  (define runbalanced
    (tree:red A (tree:black B C k1 v1) k0 v0)
    -> (:tuple (rbalance A (tree:red B C k1 v1) k0 v0) #f)
    (tree:black A (tree:black B C k1 v1) k0 v0)
    -> (:tuple (rbalance A (tree:red B C k1 v1) k0 v0) #t)
    (tree:black A (tree:red (tree:black B C k1 v1) D k2 v2) k0 v0)
    -> (:tuple (tree:black (rbalance A (tree:red B C k1 v1) k0 v0) D k2 v2) #f)
    _ -> (impossible)
    )

  ;; XXX would like to have a tree/delete-min, but this helper
  ;;    for remove-aux is not exactly that.
  (define remove-min
    (tree:empty)
    -> (impossible)
    (tree:black (tree:empty) (tree:black _ _ _ _) _ _)
    -> (impossible)
    (tree:black (tree:empty) (tree:empty) k0 v0)
    -> (:tuple (tree:empty) k0 v0 #t)
    (tree:black (tree:empty) (tree:red L R k1 v1) k0 v0)
    -> (:tuple (tree:black L R k1 v1) k0 v0 #f)
    (tree:red (tree:empty) R k0 v0)
    -> (:tuple R k0 v0 #f)
    (tree:black L R k0 v0)
    -> (let-values (((L k1 v1 d) (remove-min L)))
	 (let ((m (tree:black L R k0 v0)))
	   (if d
	       (let-values (((s d) (runbalanced m)))
		 (:tuple s k1 v1 d))
	       (:tuple m k1 v1 #f))))
    (tree:red L R k0 v0)
    -> (let-values (((L k1 v1 d) (remove-min L)))
	 (let ((m (tree:red L R k0 v0)))
	   (if d
	       (let-values (((s d) (runbalanced m)))
		 (:tuple s k1 v1 d))
	       (:tuple m k1 v1 #f))))
    )

  (define blackify
    (tree:red L R k0 v0) -> (:tuple (tree:black L R k0 v0) #f)
    m                    -> (:tuple m #t))

  (define remove-aux
    (tree:empty)
    -> (:tuple (tree:empty) #f)
    (tree:black L R k0 v0)
    -> (match (cmp k k0) with
         (cmp:<) -> (let-values (((L d) (remove-aux L)))
                      (let ((m (tree:black L R k0 v0)))
                        (if d (runbalanced m) (:tuple m #f))))
         (cmp:>) -> (let-values (((R d) (remove-aux R)))
                      (let ((m (tree:black L R k0 v0)))
                        (if d (lunbalanced m) (:tuple m #f))))
         (cmp:=) -> (match R with
                      (tree:empty) -> (blackify L)
                      _ -> (let-values (((R k0 v0 d) (remove-min R)))
                             (let ((m (tree:black L R k0 v0)))
                               (if d (lunbalanced m) (:tuple m #f))))))
    (tree:red L R k0 v0)
    -> (match (cmp k k0) with
         (cmp:<) -> (let-values (((L d) (remove-aux L)))
                      (let ((m (tree:red L R k0 v0)))
                        (if d (runbalanced m) (:tuple m #f))))
         (cmp:>) -> (let-values (((R d) (remove-aux R)))
                      (let ((m (tree:red L R k0 v0)))
                        (if d (lunbalanced m) (:tuple m #f))))
         (cmp:=) -> (match R with
                      (tree:empty) -> (:tuple L #f)
                      _ -> (let-values (((R k0 v0 d) (remove-min R)))
                             (let ((m (tree:red L R k0 v0)))
                               (if d (lunbalanced m) (:tuple m #f)))))))
  (remove-aux root)
  )

(define (tree/delete root cmp k)
  (match (tree/delete-check root cmp k) with
    (:tuple result _) -> result))

(defmacro tree/delete!
  (tree/delete! root cmp k)
  -> (set! root (tree/delete root cmp k))
  )

(define tree/black-height
  (tree:empty) acc	   -> acc
  (tree:red L _ _ _) acc   -> (tree/black-height L acc)
  (tree:black L _ _ _) acc -> (tree/black-height L (+ 1 acc))
  )

(define (tree/verify t)
  (define V
    (tree:empty) bh0 bh1			-> (= bh0 bh1)
    (tree:red (tree:red _ _ _ _) _ _ _) bh0 bh1 -> #f
    (tree:red _ (tree:red _ _ _ _) _ _) bh0 bh1 -> #f
    (tree:red L R _ _) bh0 bh1			-> (and (V L bh0 bh1) (V R bh0 bh1))
    (tree:black L R _ _) bh0 bh1                -> (and (V L bh0 (+ 1 bh1)) (V R bh0 (+ 1 bh1)))
    )
  (V t (tree/black-height t 0) 0))

(define (tree/member root cmp key)
  (let member0 ((n root))
    (match n with
      (tree:empty)
      -> (maybe:no)

      (tree:red l r k v)
      -> (match (cmp key k) with
           (cmp:<) -> (member0 l)
           (cmp:>) -> (member0 r)
           (cmp:=) -> (maybe:yes v))

      (tree:black l r k v)
      -> (match (cmp key k) with
           (cmp:<) -> (member0 l)
           (cmp:>) -> (member0 r)
           (cmp:=) -> (maybe:yes v))
      )))

(define (tree/get root cmp key)
  (match (tree/member root cmp key) with
    (maybe:yes val) -> val
    (maybe:no) -> (raise (:KeyError))
    ))

(define tree/min
  (tree:empty) -> (raise (:KeyError))
  (tree:black (tree:empty) _ k v) -> (:tuple k v)
  (tree:red   (tree:empty) _ k v) -> (:tuple k v)
  (tree:black L _ _ _) -> (tree/min L)
  (tree:red   L _ _ _) -> (tree/min L)
  )

(define tree/max
  (tree:empty) -> (raise (:KeyError))
  (tree:black _ (tree:empty) k v) -> (:tuple k v)
  (tree:red   _ (tree:empty) k v) -> (:tuple k v)
  (tree:black _ R _ _) -> (tree/max R)
  (tree:red   _ R _ _) -> (tree/max R)
  )

(define tree/inorder
  _ (tree:empty)         -> #u
  p (tree:red l r k v)   -> (begin (tree/inorder p l) (p k v) (tree/inorder p r) #u)
  p (tree:black l r k v) -> (begin (tree/inorder p l) (p k v) (tree/inorder p r) #u)
  )

(define tree/reverse
  _ (tree:empty)         -> #u
  p (tree:red l r k v)   -> (begin (tree/reverse p r) (p k v) (tree/reverse p l) #u)
  p (tree:black l r k v) -> (begin (tree/reverse p r) (p k v) (tree/reverse p l) #u)
  )

(define tree/size
  (tree:empty)         -> 0
  (tree:red l r _ _)   -> (+ 1 (+ (tree/size l) (tree/size r)))
  (tree:black l r _ _) -> (+ 1 (+ (tree/size l) (tree/size r))))

(define tree/empty?
  (tree:empty) -> #t
  _            -> #f
  )

(defmacro tree/make
  (tree/make cmp)                     -> (tree:empty)
  (tree/make cmp (k0 v0) (k1 v1) ...) -> (tree/insert (tree/make cmp (k1 v1) ...) cmp k0 v0)
  )

(defmacro tree/insert!
  (tree/insert! root cmp k v) -> (set! root (tree/insert root cmp k v)))

(defmacro for-map
  (for-map k v map body ...)
  -> (tree/inorder (lambda (k v) body ...) map)
  )

;; some way to do these using foldr?
(define (tree/keys t)
  (let ((r '()))
    (tree/reverse (lambda (k v) (PUSH r k)) t)
    r))

(define (tree/values t)
  (let ((r '()))
    (tree/reverse (lambda (k v) (PUSH r v)) t)
    r))

(define tree/dump
  d p (tree:empty)         -> #u
  d p (tree:red l r k v)   -> (begin (tree/dump (+ d 1) p l) (p k v d) (tree/dump (+ d 1) p r))
  d p (tree:black l r k v) -> (begin (tree/dump (+ d 1) p l) (p k v d) (tree/dump (+ d 1) p r))
  )

(define (tree/make-generator t)
  (make-generator
   (lambda (consumer)
     (tree/inorder
      (lambda (k v)
	(consumer (maybe:yes (:tuple k v))))
      t)
     (forever (consumer (maybe:no))))))

;; do a range query [lo, hi) over the map/set.
(define (tree/range root < lo hi p)

  (define (visit l r k v)
    (if (< lo k) (walk l))
    (if (and (< k hi) (not (< k lo))) (p k v))
    (if (< k hi) (walk r))
    )

  (define walk
    (tree:empty)         -> #u
    (tree:red   l r k v) -> (visit l r k v)
    (tree:black l r k v) -> (visit l r k v)
    )

  (walk root)
  )

(define tree/least
  (tree:empty)                     -> (error "tree/least on empty tree")
  (tree:red (tree:empty) _ k v)    -> (:tuple k v)
  (tree:black (tree:empty) _ k v)  -> (:tuple k v)
  (tree:red l _ _ _)               -> (tree/least l)
  (tree:black l _ _ _)             -> (tree/least l)
  )

(define (tree/pop-least s cmp)
  (let-values (((k v) (tree/least s)))
    (:tuple (:tuple k v) (tree/delete s cmp k))))

(defmacro tree/pop-least!
  (tree/pop-least! s cmp)
  -> (let-values (((least s0) (tree/pop-least s cmp)))
       (set! s s0)
       least))
