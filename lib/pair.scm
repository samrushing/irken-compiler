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

;; http://groups.google.com/group/comp.lang.scheme/msg/0055f311d1e1ce08

(define (reverse-onto l1 l2)
  (vcase list l1
    ((:nil) l2)
    ((:cons hd tl)
     (reverse-onto tl (list:cons hd l2)))
    ))

(define (reverse l)
  (reverse-onto l (list:nil)))

(define (append list1 list2)
  (reverse-onto (reverse list1) list2))

(define (length l)
  (define fun
    () acc	  -> acc
    (hd . tl) acc -> (fun tl (+ 1 acc)))
  (fun l 0))

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

(define nth
  ()       _ -> (error "list index out of range")
  (hd . _) 0 -> hd
  (_ . tl) n -> (nth tl (- n 1))
  )

(define (range n)
  (let loop ((n n)
	     (l (list:nil)))
    (if (= n 0)
	l
	(loop (- n 1) (cons n l)))))

(define (n-of n x)
  (let loop ((n n)
	     (l (list:nil)))
    (if (= n 0)
	l
	(loop (- n 1) (cons x l)))))

(define map
  p ()        -> '()
  p (hd . tl) -> (list:cons (p hd) (map p tl)))

(define for-each
  p ()        -> #u
  p (hd . tl) -> (begin (p hd) (for-each p tl)))

(define fold
  p acc ()	  -> acc
  p acc (hd . tl) -> (fold p (p hd acc) tl)
  )

(define foldr
  p acc ()	  -> acc
  p acc (hd . tl) -> (p hd (foldr p acc tl))
  )

(define some
  p () -> #f
  p (hd . tl) -> (if (p hd) #t (some p tl)))

(define every
  p () -> #t
  p (hd . tl) -> (if (p hd) (every p tl) #f))

;; print a list with <proc>, and print <sep> between each item.
(define print-sep
  proc sep ()	     -> #u
  proc sep (one)     -> (proc one)
  proc sep (hd . tl) -> (begin (proc hd) (print-string sep) (print-sep proc sep tl)))

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
		      (v (%make-vector n x)))
		  (recur v 0 l))))

;; using %vec16-set because the type system keeps <recur>
;;   generic, thus skipping the vec16 detection.  gotta figure this out.
(define (list->vec16 l)
  (define recur
    v _ ()      -> v
    v n (x . y) -> (begin (%vec16-set v n x) (recur v (+ n 1) y)))
  (match l with
    ()       -> #()  ;; special-case test for empty list
    (_ . _)  -> (let ((n (length l))
		      (v (%make-vec16 n)))
		  (recur v 0 l))))
