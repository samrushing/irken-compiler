
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
