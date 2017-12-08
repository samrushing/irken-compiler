;; -*- Mode: Irken -*-

;; sets implemented with the frb tree.

(define (set/empty)
  (tree:empty)
  )

(define set/empty?
  s -> (eq? s (set/empty))
  )

(define (set/size s)
  (tree/size s))

(define (set/addv s cmp k v)
  (match (tree/member s cmp k) with
    (maybe:no)    -> (tree/insert s cmp k v)
    (maybe:yes _) -> s
    ))

(define (set/add s cmp k)
  (set/addv s cmp k #f)
  )

(define set/delete tree/delete)

(defmacro set/delete!
  (set/delete! s cmp k)
  -> (set! s (set/delete s cmp k)))

(defmacro set/add*
  (set/add* s cmp) -> s
  (set/add* s cmp item items ...) -> (set/add (set/add* s cmp items ...) cmp item)
  )

(defmacro set/add!
  (set/add! s cmp x ...)
  -> (set! s (set/add* s cmp x ...))
  )

(defmacro set/make
  (set/make cmp a ...)
  -> (set/add* (set/empty) cmp a ...)
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

;; make this one nary as well?
(defmacro set/addv!
  (set/add! s cmp k v)
  -> (set! s (set/addv s cmp k v))
  )

(define (set/getkey root cmp key)
  (let member0 ((n root))
    (match n with
      (tree:empty)
      -> (maybe:no)
      (tree:red l r k v)
      -> (match (cmp key k) with
           (cmp:<) -> (member0 l)
           (cmp:>) -> (member0 r)
           (cmp:=) -> (maybe:yes k))
      (tree:black l r k v)
      -> (match (cmp key k) with
           (cmp:<) -> (member0 l)
           (cmp:>) -> (member0 r)
           (cmp:=) -> (maybe:yes k))
      )))

(define (set/member s cmp k)
  (match (tree/member s cmp k) with
    (maybe:yes _) -> #t
    (maybe:no)    -> #f
    ))

(define (set/iterate s p)
  (tree/inorder (lambda (k _) (p k)) s))

(define (set/make-generator t)
  (make-generator
   (lambda (consumer)
     (tree/inorder (lambda (k v) (consumer (maybe:yes k))) t)
     (forever (consumer (maybe:no))))))

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

(define (set->list s)
  (let ((r '()))
    (tree/reverse (lambda (k _) (PUSH r k)) s)
    r))

(defmacro for-set
  (for-set vname set body ...)
  -> (set/iterate set (lambda (vname) body ...))
  )

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
	     (loop ($g0) ($g1)))))
  )

;; this function 'lifts' a 'cmp' function to a 'set-cmp' function.
(define (make-set-cmp cmp)
  (lambda (a b)
    (set/cmp a b cmp)))

(define (set/range->list s cmp lo hi)
  (let ((r '()))
    (tree/range s cmp lo hi (lambda (k v) (PUSH r k)))
    ;; could avoid the reverse if tree/range
    ;;   went backward, but that would be counter-intuitive
    (reverse r)))

(define (set/min s)
  (match (tree/min s) with
    (:tuple k _) -> k
    ))

(define (set/pop-min s cmp)
  (let ((min (set/min s)))
    (:tuple min (set/delete s cmp min))
    ))

(defmacro set/pop-min!
  (set/pop-min! s cmp)
  -> (let-values (((min s0) (set/pop-min s cmp)))
       (set! s s0)
       min))

(define (set/intersection cmp a b)
  (let ((result (set/empty)))
    (for-set x a
      (if (set/member b cmp x)
	  (set/add! result cmp x)))
    result))

(define (set/union cmp a b)
  (let ((result b))
    (for-set x a
      (set/add! result cmp x))
    result))

(define (set/difference cmp a b)
  (let ((result a))
    (for-set x b
      (set/delete! result cmp x))
    result))

(define (set-map s cmp p)
  (let ((r (set/empty)))
    (for-set x s
      (set/add! r cmp (p x)))
    r))
