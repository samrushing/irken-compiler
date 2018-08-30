;; -*- Mode: Irken -*-

;; mostly identical to lib/frb.scm

(datatype set
  (:red   (set 'a) (set 'a) 'a)
  (:black (set 'a) (set 'a) 'a)
  (:empty)
  )

(define (set/empty) (set:empty))

(define set/lbalance
  (set:red (set:red A B k0) C k1) D k2 -> (set:red (set:black A B k0) (set:black C D k2) k1)
  (set:red A (set:red B C k1) k0) D k2 -> (set:red (set:black A B k0) (set:black C D k2) k1)
                                 A B k -> (set:black A B k))

(define set/rbalance
  A (set:red (set:red B C k1) D k2) k0 -> (set:red (set:black A B k0) (set:black C D k2) k1)
  A (set:red B (set:red C D k2) k1) k0 -> (set:red (set:black A B k0) (set:black C D k2) k1)
                                 A B k -> (set:black A B k))

(define (set/insert root cmp k)

  (define (ins n)
    (match n with
      (set:empty)
      -> (set:red (set:empty) (set:empty) k)

      (set:red l r k2)
      -> (match (cmp k k2) with
           (cmp:<) -> (set:red (ins l) r k2)
           (cmp:>) -> (set:red l (ins r) k2)
           (cmp:=) -> n
           )
      (set:black l r k2)
      -> (match (cmp k k2) with
           (cmp:<) -> (set/lbalance (ins l) r k2)
           (cmp:>) -> (set/rbalance l (ins r) k2)
           (cmp:=) -> n
           )))

  (let ((s (ins root)))
    (match s with
      (set:red l r k0) -> (set:black l r k0)
      _ -> s
      ))

  )

(define (set/one item)
  (set:red (set:empty) (set:empty) item))

;; deletion translated from https://github.com/bmeurer/ocaml-rbsets

(define (set/delete-check root cmp k)

  (define lunbalanced
    (set:red (set:black A B k0) C k1)
    -> (:tuple (set/lbalance (set:red A B k0) C k1) #f)
    (set:black (set:black A B k0) C k1)
    -> (:tuple (set/lbalance (set:red A B k0) C k1) #t)
    (set:black (set:red A (set:black B C k1) k0) D k2)
    -> (:tuple (set:black A (set/lbalance (set:red B C k1) D k2) k0) #f)
    _ -> (impossible)
    )

  (define runbalanced
    (set:red A (set:black B C k1) k0)
    -> (:tuple (set/rbalance A (set:red B C k1) k0) #f)
    (set:black A (set:black B C k1) k0)
    -> (:tuple (set/rbalance A (set:red B C k1) k0) #t)
    (set:black A (set:red (set:black B C k1) D k2) k0)
    -> (:tuple (set:black (set/rbalance A (set:red B C k1) k0) D k2) #f)
    _ -> (impossible)
    )

  ;; XXX would like to have a set/delete-min, but this helper
  ;;    for remove-aux is not exactly that.
  (define remove-min
    (set:empty)
    -> (impossible)
    (set:black (set:empty) (set:black _ _ _) _)
    -> (impossible)
    (set:black (set:empty) (set:empty) k0)
    -> (:tuple (set:empty) k0 #t)
    (set:black (set:empty) (set:red L R k1) k0)
    -> (:tuple (set:black L R k1) k0 #f)
    (set:red (set:empty) R k0)
    -> (:tuple R k0 #f)
    (set:black L R k0)
    -> (let (((L k1 d) (remove-min L))
             (m (set:black L R k0)))
         (if d
             (let (((s d) (runbalanced m)))
               (:tuple s k1 d))
             (:tuple m k1 #f)))
    (set:red L R k0)
    -> (let (((L k1 d) (remove-min L))
             (m (set:red L R k0)))
         (if d
             (let (((s d) (runbalanced m)))
               (:tuple s k1 d))
             (:tuple m k1 #f)))
    )

  (define blackify
    (set:red L R k0) -> (:tuple (set:black L R k0) #f)
    m                -> (:tuple m #t))

  (define remove-aux
    (set:empty)
    -> (:tuple (set:empty) #f)
    (set:black L R k0)
    -> (match (cmp k k0) with
         (cmp:<) -> (let (((L d) (remove-aux L))
                          (m (set:black L R k0)))
                      (if d (runbalanced m) (:tuple m #f)))
         (cmp:>) -> (let (((R d) (remove-aux R))
                          (m (set:black L R k0)))
                      (if d (lunbalanced m) (:tuple m #f)))
         (cmp:=) -> (match R with
                      (set:empty) -> (blackify L)
                      _ -> (let (((R k0 d) (remove-min R))
                                 (m (set:black L R k0)))
                             (if d (lunbalanced m) (:tuple m #f)))))
    (set:red L R k0)
    -> (match (cmp k k0) with
         (cmp:<) -> (let (((L d) (remove-aux L))
                          (m (set:red L R k0)))
                      (if d (runbalanced m) (:tuple m #f)))
         (cmp:>) -> (let (((R d) (remove-aux R))
                          (m (set:red L R k0)))
                      (if d (lunbalanced m) (:tuple m #f)))
         (cmp:=) -> (match R with
                      (set:empty) -> (:tuple L #f)
                      _ -> (let (((R k0 d) (remove-min R))
                                 (m (set:red L R k0)))
                             (if d (lunbalanced m) (:tuple m #f))))))
  (remove-aux root)
  )

(define (set/delete root cmp k)
  (match (set/delete-check root cmp k) with
    (:tuple result _) -> result))

(defmacro set/delete!
  (set/delete! root cmp k)
  -> (set! root (set/delete root cmp k))
  )

(define set/black-height
  (set:empty) acc	-> acc
  (set:red L _ _) acc   -> (set/black-height L acc)
  (set:black L _ _) acc -> (set/black-height L (+ 1 acc))
  )

(define (set/verify t)
  (define V
    (set:empty) bh0 bh1		    -> (= bh0 bh1)
    (set:red (set:red _ _ _) _ _) bh0 bh1 -> #f
    (set:red _ (set:red _ _ _) _) bh0 bh1 -> #f
    (set:red L R _) bh0 bh1		    -> (and (V L bh0 bh1) (V R bh0 bh1))
    (set:black L R _) bh0 bh1              -> (and (V L bh0 (+ 1 bh1)) (V R bh0 (+ 1 bh1)))
    )
  (V t (set/black-height t 0) 0))

(define (set/member? root cmp key)
  (let member0 ((n root))
    (match n with
      (set:empty)
      -> #f
      (set:red l r k)
      -> (match (cmp key k) with
           (cmp:<) -> (member0 l)
           (cmp:>) -> (member0 r)
           (cmp:=) -> #t)
      (set:black l r k)
      -> (match (cmp key k) with
           (cmp:<) -> (member0 l)
           (cmp:>) -> (member0 r)
           (cmp:=) -> #t)
      )))

(define set/min
  (set:empty) -> (raise (:Set/Empty))
  (set:black (set:empty) _ k) -> k
  (set:red   (set:empty) _ k) -> k
  (set:black L _ _) -> (set/min L)
  (set:red   L _ _) -> (set/min L)
  )

(define set/max
  (set:empty) -> (raise (:Set/Empty))
  (set:black _ (set:empty) k) -> k
  (set:red   _ (set:empty) k) -> k
  (set:black _ R _) -> (set/max R)
  (set:red   _ R _) -> (set/max R)
  )

(define set/inorder
  _ (set:empty)       -> #u
  p (set:red l r k)   -> (begin (set/inorder p l) (p k) (set/inorder p r) #u)
  p (set:black l r k) -> (begin (set/inorder p l) (p k) (set/inorder p r) #u)
  )

(define set/reverse
  _ (set:empty)       -> #u
  p (set:red l r k)   -> (begin (set/reverse p r) (p k) (set/reverse p l) #u)
  p (set:black l r k) -> (begin (set/reverse p r) (p k) (set/reverse p l) #u)
  )

(define set/size
  (set:empty)       -> 0
  (set:red l r _)   -> (+ 1 (+ (set/size l) (set/size r)))
  (set:black l r _) -> (+ 1 (+ (set/size l) (set/size r))))

(define set/empty?
  (set:empty) -> #t
  _           -> #f
  )

(defmacro set/make
  (set/make cmp)           -> (set:empty)
  (set/make cmp k0 k1 ...) -> (set/insert (set/make cmp k1 ...) cmp k0)
  )

(defmacro set/insert!
  (set/insert! root cmp k) -> (set! root (set/insert root cmp k)))

;; some way to do these using foldr?
(define (set/keys t)
  (let ((r '()))
    (set/reverse (lambda (k) (push! r k)) t)
    r))

(define set/dump
  d p (set:empty)       -> #u
  d p (set:red l r k)   -> (begin (set/dump (+ d 1) p l) (p k d) (set/dump (+ d 1) p r))
  d p (set:black l r k) -> (begin (set/dump (+ d 1) p l) (p k d) (set/dump (+ d 1) p r))
  )

(define (set/make-generator t)
  (makegen emit
    (set/inorder emit t)))

(define (set/make-reverse-generator t)
  (makegen emit
    (set/reverse emit t)))

;; do a range query [lo, hi) over the map/set.
(define (set/range root cmp lo hi p)

  (define (< a b)
    (eq? (cmp:<) (cmp a b)))

  (define (visit l r k)
    (if (< lo k) (walk l))
    (if (and (< k hi) (not (< k lo))) (p k))
    (if (< k hi) (walk r))
    )

  (define walk
    (set:empty)       -> #u
    (set:red   l r k) -> (visit l r k)
    (set:black l r k) -> (visit l r k)
    )

  (walk root)
  )

(define (set/pop-min s cmp)
  (let ((k (set/min s)))
    (:tuple k (set/delete s cmp k))))

(defmacro set/pop-min!
  (set/pop-min! s cmp)
  -> (let (((min s0) (set/pop-min s cmp)))
       (set! s s0)
       min))

(define (set/add s cmp k)
  (if (set/member? s cmp k)
      s
      (set/insert s cmp k)))

(defmacro set/add*
  (set/add* s cmp) -> s
  (set/add* s cmp item items ...) -> (set/add (set/add* s cmp items ...) cmp item)
  )

(defmacro set/add!
  (set/add! s cmp x ...)
  -> (set! s (set/add* s cmp x ...))
  )

(defmacro for-set
  (for-set vname set body ...)
  -> (set/inorder (lambda (vname) body ...) set)
  )

;; remove x, then add a ...
(defmacro set/replace
  (set/replace s cmp x a ...)
  -> (set/add* (set/delete s cmp x) cmp a ...)
  )

(defmacro set/replace!
  (set/replace! s cmp x a ...)
  -> (set! s (set/replace s cmp x a ...))
  )

(define (set/getkey root cmp key)
  (let member0 ((n root))
    (match n with
      (set:empty)
      -> (maybe:no)
      (set:red l r k)
      -> (match (cmp key k) with
           (cmp:<) -> (member0 l)
           (cmp:>) -> (member0 r)
           (cmp:=) -> (maybe:yes k))
      (set:black l r k)
      -> (match (cmp key k) with
           (cmp:<) -> (member0 l)
           (cmp:>) -> (member0 r)
           (cmp:=) -> (maybe:yes k))
      )))

;; real-world use of same-fringe!
(define (set/cmp a b cmp)
  (let ((g0 (set/make-generator a))
        (g1 (set/make-generator b)))
    (let loop ()
      (match (g0) (g1) with
        (maybe:no)    (maybe:no)    -> (cmp:=)
        (maybe:no)    (maybe:yes _) -> (cmp:<)
        (maybe:yes _) (maybe:no)    -> (cmp:>)
        (maybe:yes ia) (maybe:yes ib)
        -> (match (cmp ia ib) with
             (cmp:<) -> (cmp:<)
             (cmp:>) -> (cmp:>)
             (cmp:=) -> (loop))
        ))
    ))

(define list->set
  ()        cmp acc -> acc
  (hd . tl) cmp acc -> (list->set tl cmp (set/add acc cmp hd))
  )

;; XXX fold?
(define (set->list s)
  (let ((r '()))
    (set/reverse (lambda (k) (push! r k)) s)
    r))

(defmacro for-set2
  (for-set2 v0 v1 set0 set1 body ...)
  -> (let (($g0 (set/make-generator set0))
	   ($g1 (set/make-generator set1)))
       (let/cc $return
	   (let loop (($m0 ($g0))
		      ($m1 ($g1)))
	     (match $m0 $m1 with
	       (maybe:no) (maybe:no)
	       -> ($return #u)
	       (maybe:yes v0) (maybe:yes v1)
	       -> (begin body ...)
	       _ _
	       -> (error "unequal set lengths")
	       )
	     (loop ($g0) ($g1))))))

;; this function 'lifts' a 'cmp' function to a 'set-cmp' function.
(define (make-set-cmp cmp)
  (lambda (a b)
    (set/cmp a b cmp)))

(define (set/range->list s cmp lo hi)
  (let ((r '()))
    (set/range s cmp lo hi (lambda (k) (push! r k)))
    (reverse r)))

(define (set/fold acc s cmp p)
  (define walk
    acc (set:empty)       -> acc
    acc (set:red   l r k) -> (walk (p (walk acc l) k) r)
    acc (set:black l r k) -> (walk (p (walk acc l) k) r)
    )
  (walk acc s)
  )

(define (set/union cmp a b)
  (set/fold
   a b cmp
   (lambda (acc k)
     (set/add acc cmp k))))

(define (set/difference cmp a b)
  (set/fold
   a b cmp
   (lambda (acc k)
     (set/delete acc cmp k))))

(define (set/intersection cmp a b)
  (set/fold
   (set/empty) a cmp
   (lambda (acc k)
     (if (set/member? b cmp k)
         (set/add acc cmp k)
         acc))))

(define (set/map s cmp p)
  (set/fold
   (set/empty) s cmp
   (lambda (acc k)
     (set/add acc cmp (p k)))))
