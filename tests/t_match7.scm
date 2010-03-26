
(include "lib/core.scm")
(include "lib/random.scm")

(datatype T
  (:E)
  (:R (T 'a) 'a (T 'a))
  (:B (T 'a) 'a (T 'a))
  )

(define (T/insert root < k)

  (define lbalance
    (T:R (T:R a x b) y c) z d -> (T:R (T:B a x b) y (T:B c z d))
    (T:R a x (T:R b y c)) z d -> (T:R (T:B a x b) y (T:B c z d))
    a x b                     -> (T:B a x b)
    )

  (define rbalance
    a x (T:R (T:R b y c) z d) -> (T:R (T:B a x b) y (T:B c z d))
    a x (T:R b y (T:R c z d)) -> (T:R (T:B a x b) y (T:B c z d))
    a x b                     -> (T:B a x b)
    )

  (define (ins n)
    (vcase T n
      ((:E)
       (T:R (T:E) k (T:E)))
      ((:R l k2 r)
       (cond ((< k k2)
	      (T:R (ins l) k2 r))
	     ((< k2 k)
	      (T:R l k2 (ins r)))
	     (else n)))
      ((:B l k2 r)
       (cond ((< k k2)
	      (lbalance (ins l) k2 r))
	     ((< k2 k)
	      (rbalance l k2 (ins r)))
	     (else n)))))

  (let ((s (ins root)))
    (vcase T s
      ((:B _ _ _) s)
      ((:R l k r) (T:B l k r))
      ((:E) s) ;; impossible, should raise something here?
      )))

(define (print-spaces n)
  (let loop ((n n))
    (cond ((> n 0)
	   (print-string "  ")
	   (loop (- n 1))))))

(define (print-item x d)
  (print-spaces d)
  (printn x))

(define T/print
  d (T:E)       -> #u
  d (T:R l x r) -> (begin (T/print (+ d 1) l) (print-item x d) (T/print (+ d 1) r))
  d (T:B l x r) -> (begin (T/print (+ d 1) l) (print-item x d) (T/print (+ d 1) r))
  )

(define (n-random n)
  (let loop ((n n)
	     (t (T:E)))
    (if (= n 0)
	t
	(loop (- n 1) (T/insert t < (random))))))

(T/print 0 (n-random 20))

