;; this needs to be renamed to 'list.scm'

(datatype list
  (:nil)
  (:cons 'a (list 'a))
  )

(define (null? l)
  (vcase list l
    ((:nil) #t)
    ;; could/should use 'else' here.
    ((:cons _ _ ) #f)))

(define (cons a b)
  (list:cons a b))

(define car
  () -> (error "car")
  (x . _) -> x)

(define cdr
  () -> (error "cdr")
  (_ . y) -> y)

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
  (let loop ((l l)
	     (n 0))
    (vcase list l
       ((:nil) n)
       ((:cons _ tl)
	(loop tl (+ n 1))))))

(define (member x l =)
  (vcase list l
    ((:nil) #f)
    ((:cons hd tl)
     (if (= hd x)
	 #t
	 (member x tl =)))))

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

(define (map p l)
  (vcase list l
    ((:nil) l)
    ((:cons hd tl)
     (list:cons (p hd) (map p tl)))))

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
