
;; maybe one way of doing OO-like stuff using records?

(include "lib/core.scm")

(define (make-counter)
  (let ((val 0))
    (define (next)
      ;((lambda (r) (set! val (+ val 1)) r) val)
      (let ((r val))
	(set! val (+ val 1))
	r)
      )
    {next=next}
    ))

;(let ((r val))
;  (set! val (+ val 1))
;  r)
;((lambda (r) (set! val (+ val 1)) r) val)

(define (n-counters n)
  (let loop ((n n) (l (:nil)))
    (if (zero? n)
	l
	(loop (- n 1) (:cons (make-counter) l)))))

(n-counters 5)

;(let ((x (make-counter))
;      (y (make-counter)))
;;   (printn (x.next))
;;   (printn (x.next))
;;   (printn (x.next))
;;   (printn (x.next))
;;   (printn (x.next))
;;   (printn (x.next))
;;   (printn (y.next))
;;   (printn (y.next))
;;   (printn (y.next))
;;   (printn (y.next))
;;   (printn (y.next))
;  (printn (y.next))
;  )
