
(define (= a b)
  (%%cexp (int int -> bool) "%s==%s" a b))

(define (+ a b)
  (%%cexp (int int -> int) "%s+%s" a b))

(define (- a b)
  (%%cexp (int int -> int) "%s-%s" a b))

(define (null? l)
  (vcase l
    ((:nil) #t)
    ((:cons _ _ ) #f)))

;; http://groups.google.com/group/comp.lang.scheme/msg/0055f311d1e1ce08

(define (reverse-onto l1 l2)
  (vcase l1
    ((:nil) l2)
    ((:cons hd tl)
     (reverse-onto tl (:cons hd l2)))
    ))

(define (reverse l)
  (reverse-onto l (:nil)))

(define (append list1 list2)
  (reverse-onto (reverse list1) list2))

(define (length l)
  (let loop ((l l)
	     (n 0))
    (vcase l
       ((:nil) n)
       ((:cons _ tl)
	(loop tl (+ n 1))))))

(define (member x l =)
  (vcase l
    ((:nil) #f)
    ((:cons hd tl)
     (if (= hd x)
	 #t
	 (member x tl =)))))

(define (range n)
  (let loop ((n n)
	     (l (:nil)))
    (if (= n 0)
	l
	(loop (- n 1) (:cons n l)))))

(define (n-of n x)
  (let loop ((n n)
	     (l (:nil)))
    (if (= n 0)
	l
	(loop (- n 1) (:cons x l)))))

(let ((l (:cons 1 (:cons 2 (:cons 3 (:nil))))))
  (let ((t0 (member 3 l =))
	(t1 (length l))
	(t2 (append l l))
	(t3 (range 10))
	(t4 (n-of 5 "five"))
	(t5 (reverse t3))
	)
    {a=t0 b=t1 c=t2 d=t3 e=t4 f=t5}))
