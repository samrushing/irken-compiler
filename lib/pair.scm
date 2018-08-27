;; -*- Mode: Irken -*-

;; this needs to be renamed to 'list.scm'

(datatype list
  (:nil)
  (:cons 'a (list 'a))
  )

;; null?/cons/car/cdr aren't actually used that much in Irken code,
;;   since pattern matching is safer and easier to read.
(define null?
  () -> #t
  _  -> #f
  )

(define (cons a b)
  (list:cons a b))

(define car
  () -> (error "car")
  (x . _) -> x)

(define cdr
  () -> (error "cdr")
  (_ . y) -> y)

;; I'm planning on downcasing these two eventually.  I was thinking of
;;  such macros in C-like terms - i.e., warn the user that they're macros,
;;  but it just annoyingly sticks out.
(defmacro LIST
  (LIST)         -> (list:nil)
  (LIST x y ...) -> (list:cons x (LIST y ...)))

(defmacro PUSH
  (PUSH l v)     -> (set! l (list:cons v l))
  )

(defmacro pop
  (pop l) -> (match l with
	       (list:nil) -> (error "pop")
	       (list:cons hd tl) -> (begin (set! l tl) hd)))

(defmacro prepend
  (prepend l)	    -> l
  (prepend a b ...) -> (list:cons a (prepend b ...)))

;; http://groups.google.com/group/comp.lang.scheme/msg/0055f311d1e1ce08

;; (a b c) (d e f) => (c b a d e f)
(define reverse-onto
  () b	      -> b
  (hd . tl) b -> (reverse-onto tl (list:cons hd b)))

(define (reverse l)
  (reverse-onto l '()))

(define (append2 list1 list2)
  (reverse-onto (reverse list1) list2))

(defmacro append
  (append l)	   -> l
  (append a b ...) -> (append2 a (append b ...))
  )

(defmacro append!
  (append! l0 l1 ...)
  -> (set! l0 (append l0 l1 ...))
  )

(define length*
  ()        acc -> acc
  (_ . tl) acc -> (length* tl (+ 1 acc)))

(define (length l)
  (length* l 0))

(define (first x) (car x))
(define (rest x) (cdr x))
(define (second l) (car (cdr l)))
(define last
  ()	   -> (error "last")
  (last)   -> last
  (_ . tl) -> (last tl))
(define (butlast l)
  (let loop ((l l) (acc '()))
    (match l with
      ()	-> acc
      (_)	-> (reverse acc)
      (hd . tl) -> (loop tl (list:cons hd acc))
      )))

;; A possible pattern-matching named-let construct?
;; (define (length l)
;;   (let loop (0 l)
;;     acc ()	  -> acc
;;     acc (hd . tl) -> (loop tl (+ 1 acc))))

;; this is different enough from the scheme <member> to warrant
;;   the new name.

(define member?
  x ()        = -> #f
  x (hd . tl) = -> (if (= hd x) #t (member? x tl =))
  )

;; XXX need to get inlining to work through this
(define member-eq?
  x ()	      -> #f
  x (hd . tl) -> (if (eq? x hd) #t (member-eq? x tl))
  )

(define remove-eq
  x () -> '()
  x (hd . tl) -> (if (eq? hd x)
		     (remove-eq x tl)
		     (list:cons hd (remove-eq x tl))))

;; note: not tail-recursive
(define remove
  eq? item ()        -> '()
  eq? item (hd . tl) -> (if (eq? hd item)
                            (remove eq? item tl)
                            (list:cons hd (remove eq? item tl)))
  )

(defmacro remove!
  (remove! item list)
  -> (set! list (remove-eq item list))
  (remove! item list eq?)
  -> (set! list (remove eq? item list))
  )

(define nth
  ()       _ -> (error "list index out of range")
  (hd . _) 0 -> hd
  (_ . tl) n -> (nth tl (- n 1))
  )

(define take
  xs       0 -> (list:nil)
  (x . xs) n -> (list:cons x (take xs (- n 1)))
  ()       _ -> (error "list index out of range")
  )

(define drop
  ()       _ -> (error "list index out of range")
  xs       0 -> xs
  (_ . xs) n -> (drop xs (- n 1))
  )

;; (a b c d e f) 3 x   -> (a b c x d e f)
;; (d e f) 0 x (c b a) -> (a b c x d e f)

(define insert*
  (hd . tl) 0 item acc -> (reverse-onto acc (list:cons item (list:cons hd tl)))
  (hd . tl) n item acc -> (insert* tl (- n 1) item (list:cons hd acc))
  ()        n item acc -> (reverse-onto acc (list:cons item (list:nil)))
  )

(define insert list n item -> (insert* list n item '()))

(defmacro insert!
  (insert! list n item)
  -> (set! list (insert list n item)))

(define (index-eq v l)
  (let loop ((i 0)
	     (l l))
    (match l with
      ()	-> (error "list index out of range")
      (hd . tl) -> (if (eq? hd v)
		       i
		       (loop (+ i 1) tl)))))

;; needed: fancy pythonic slicing with negative index, slop, etc...
(define (slice l start end)
  (if (< (- end start) 0)
      '()
      (let loop ((l l) (i 0) (r '()))
	(cond ((< i start) (loop (cdr l) (+ i 1) r))
	      ((< i end) (loop (cdr l) (+ i 1) (list:cons (car l) r)))
	      (else (reverse r))))))

;; (range 5) => '(0 1 2 3 4)
(define (range n)
  (let loop ((n (- n 1))
	     (l (list:nil)))
    (if (< n 0)
	l
	(loop (- n 1) (list:cons n l)))))

(define (n-of n x)
  (let loop ((n n)
	     (l (list:nil)))
    (if (<= n 0)
	l
	(loop (- n 1) (list:cons x l)))))

(define map
  p () -> '()
  p (hd . tl) -> (list:cons (p hd) (map p tl)))

;; could we use a macro to define nary map?
(define map2
  p () ()		    -> '()
  p (hd0 . tl0) (hd1 . tl1) -> (list:cons (p hd0 hd1) (map2 p tl0 tl1))
  p a b			    -> (error1 "map2: unequal-length lists" (:pair a b))
  )

(defmacro map-range
  (map-range vname num body ...)
  -> (let (($n num))
       (let $loop ((vname 0)
		   ($acc (list:nil)))
	 (if (= vname $n)
	     (reverse $acc)
	     ($loop (+ vname 1) (list:cons (begin body ...) $acc))))))

(define filter
  p () -> '()
  p (hd . tl) -> (if (p hd)
		     (list:cons hd (filter p tl))
		     (filter p tl)))

(define all?
  p ()        -> #t
  p (hd . tl) -> (if (p hd) (all? p tl) #f)
  )

(define any?
  p ()        -> #f
  p (hd . tl) -> (if (p hd) #t (any? p tl))
  )

;; it's a shame that for-each puts the procedure first,
;;   definitely hurts readability when using a lambda.
(define for-each
  p ()        -> #u
  p (hd . tl) -> (begin (p hd) (for-each p tl)))

(define for-each2
  p () ()		-> #u
  p (h0 . t0) (h1 . t1) -> (begin (p h0 h1) (for-each2 p t0 t1))
  p _ _			-> (error "for-each2: unequal-length lists")
  )

(defmacro for-list
  (for-list vname list body ...)
  -> (let $loop (($list list))
       (match $list with
	 () -> #u
	 (vname . $tl) -> (begin body ... ($loop $tl))
	 )))

(defmacro for-list2
  (for-list v0 v1 l0 l1 body ...)
  -> (for-each2 (lambda (v0 v1) body ...) l0 l1)
  )

(define fold
  p acc ()	  -> acc
  p acc (hd . tl) -> (fold p (p hd acc) tl)
  )

(define foldr
  p acc ()	  -> acc
  p acc (hd . tl) -> (p hd (foldr p acc tl))
  )

(define some?
  p () -> #f
  p (hd . tl) -> (if (p hd) #t (some? p tl)))

(define every?
  p () -> #t
  p (hd . tl) -> (if (p hd) (every? p tl) #f))

(define every2?
  p () () -> #t
  p (h0 . t0) (h1 . t1) -> (if (p h0 h1) (every2? p t0 t1) #f)
  p _ _ -> (error "every2?: unequal-length lists")
  )

(define count
  p acc () -> acc
  p acc (hd . tl)
  -> (if (p hd)
         (count p (+ 1 acc) tl)
         (count p acc tl)))

;; lexicographic ordering on lists
(define list<?
  lt? ()      () -> #f
  lt? ()       _ -> #t
  lt? (_ . _) () -> #f
  lt? (a . as) (b . bs)
  -> (cond ((lt? a b) #t)
           ((lt? b a) #f)
           (else (list<? lt? as bs))))

(define list-cmp
  cmp () () -> (cmp:=)
  cmp _  () -> (cmp:>)
  cmp () _  -> (cmp:<)
  cmp (a . as) (b . bs)
  -> (match (cmp a b) with
       (cmp:=) -> (list-cmp cmp as bs)
       result  -> result
       ))

;; collect lists of duplicate runs
;; http://www.christiankissig.de/cms/files/ocaml99/problem09.ml
;; I put in the '(reverse s)' call to make the algorithm 'stable'.
(define (pack l =)
  (define (pack2 l s e)
    (match l with
      ()      -> (LIST (reverse s))
      (h . t) -> (if (= h e)
		     (pack2 t (list:cons h s) e)
		     (list:cons (reverse s) (pack2 t (LIST h) h)))))
  (match l with
    ()	    -> '()
    (h . t) -> (pack2 t (LIST h) h)))

(define (vector->list v)
  (let loop ((n (- (vector-length v) 1)) (acc (list:nil)))
    (if (< n 0)
	acc
	(loop (- n 1) (list:cons v[n] acc)))))

(define (list->vector l)
  (define recur
    v _ ()      -> v
    v n (x . y) -> (begin (set! v[n] x) (recur v (+ n 1) y)))
  (match l with
    ()       -> #()  ;; special-case test for empty list
    (x . _)  -> (let ((n (length l))
		      (v (make-vector n x)))
		  (recur v 0 l))))

;; http://www.codecodex.com/wiki/Merge_sort#OCaml

(define (sort < l)

  (define merge
    () lb -> lb
    la () -> la
    (ha . ta) (hb . tb)
    -> (if (< ha hb)
	   (list:cons ha (merge ta (list:cons hb tb)))
	   (list:cons hb (merge (list:cons ha ta) tb))))

  (define (halve l)
    (match l with
      ()  -> (:pair l '())
      (x) -> (:pair l '())
      (hd . tl)
      -> (match (halve tl) with
	   (:pair t0 t1) -> (:pair (list:cons hd t1) t0))))

  (define (merge-sort l)
    (match l with
      ()   -> l
      (x)  -> l
      list -> (match (halve l) with
		(:pair l0 l1) -> (merge (merge-sort l0) (merge-sort l1)))))

  (merge-sort l)

  )

;; XXX obviously this needs replacing 8^)
(define (sort-vector < v)
  (list->vector (sort < (vector->list v))))

(define (list-generator L) : ((list 'a) -> (-> (maybe 'a)))
  (make-generator
   (lambda (consumer)
     (for-list item L
       (consumer (maybe:yes item)))
     (forever (consumer (maybe:no))))))

(define (generator->list gen)
  (let ((r '()))
    (for item gen
      (PUSH r item))
    (reverse r)))

;; take `n` elements from a generator into a list.
(define (gen-take n gen)
  (define loop
    1 acc (maybe:yes item) -> (reverse (list:cons item acc))
    n acc (maybe:yes item) -> (loop (- n 1) (list:cons item acc) (gen))
    n acc (maybe:no)       -> (list:nil)
    )
  (loop n '() (gen)))

