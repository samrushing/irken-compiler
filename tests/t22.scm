
;; maybe one way of doing OO-like stuff using records?

(include "lib/core.scm")
(include "lib/pair.scm")

(define (make-counter)
  (let ((val 0))
    (define (next)
      (let ((r val))
	(set! val (+ val 1))
	r)
      )
    {next=next}
    ))

(define (n-counters n)
  (let loop ((n n) (l (list:nil)))
    (if (zero? n)
	l
	(loop (- n 1) (list:cons (make-counter) l)))))

(printn (n-counters 5))

(let ((x (make-counter))
      (y (make-counter)))
  (printn (x.next))
  (printn (x.next))
  (printn (x.next))
  (printn (x.next))
  (printn (x.next))
  (printn (x.next))
  (printn (y.next))
  (printn (y.next))
  (printn (y.next))
  (printn (y.next))
  (printn (y.next))
  (printn (y.next))
  )
