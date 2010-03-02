
;; re-introducing datatype declarations for normal sum types

(datatype list
  (:nil)
  (:cons 'a (list 'a))
  )

(define (+ a b)
  (%%cexp (int int -> int) "%s+%s" a b))

(define (- a b)
  (%%cexp (int int -> int) "%s-%s" a b))

(define (zero? a)
  (%%cexp (int -> bool) "%s==0" a))

(define (length l)
  (let loop ((l l) (r 0))
    (vcase list l
       ((:nil) r)
       ((:cons _ tl) (loop tl (+ r 1))))))

(define (range n)
  (let loop ((n n) (r (list:nil)))
    (if (zero? n)
	r
	(loop (- n 1) (list:cons n r)))))

(define (n-of n x)
  (let loop ((n n) (r (list:nil)))
    (if (zero? n)
	r
	(loop (- n 1) (list:cons x r)))))

(define (member x = l)
  (let loop ((l l))
    (vcase list l
       ((:nil) #f)
       ((:cons hd tl)
	(if (= x hd)
	    #t
	    (loop tl))))))

(define (printn x)
  (%%cexp ('a -> undefined) "dump_object (%s, 0); fprintf (stdout, \"\\n\")" x))

(define (= a b)
  (%%cexp (int int -> bool) "%s==%s" a b))

(let ((l0 (range 5))
      (l1 (list:nil))
      (l2 (list:nil))
      )
  (printn l0)
  (printn (length l0))
  (printn l1)
  (printn (length l1))
  (set! l1 (list:cons #\C l1))
  (printn l1)
  (printn (length l1))
  (set! l1 (list:cons #\B l1))
  (printn l1)
  (printn (length l1))
  (set! l1 (list:cons #\A l1))
  (printn l1)
  (printn (length l1))
  (set! l2 (n-of 10 "hello"))
  (printn l2)
  (printn (member 15 = l0))
  99
  )


