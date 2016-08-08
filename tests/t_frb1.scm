;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")
(include "lib/frb.scm")

(define (t0)

  (let ((t (tree/make <
             (1 "time")
             (2 "flies")
             (3 "like")
             (4 "a")
             (5 "banana")
             )))
    (printn t)
    (tree/dump 0 (lambda (k v d) (printf (repeat d "  ") (int k) " " (string v) "\n")) t)
    (tree/member t < 5)
    (let ((t1 (tree/delete t < 4)))
      (printn t1))
    ))

(include "lib/random.scm")

(define assert
  #t -> #u
  #f -> (error "assertion failed.")
  )

(define (t1 n)

  (define (pprint t)
    (tree/dump 0 (lambda (k v d) (printf (lpad 5 (int v)) " " (repeat d "  ") (int k) "\n")) t)
    )

  (let ((t (tree:empty))
        (keys0 '()))
    (srandom 3141596)
    (for-range i n
       (let ((v (random)))
         (tree/insert! t < v i)
         (PUSH keys0 v)
         ))
    (assert (tree/verify t))
    (let ((t2 t))
      (for-each 
       (lambda (k) 
         (set! t2 (tree/delete t2 < k))
	 (assert (tree/verify t2))
         )
       keys0))
    ))

(define (t2 n)
  (let ((t (tree:empty)))
    (srandom 3141596)
    (for-range i n
       (let ((v (random)))
         (set! t (tree/insert t < v i))
         ))
    (let ((bh (tree/black-height t 0)))
      (printf "black-height: " (int bh) "\n")
      (printn (tree/verify t)))
    ))

(t0)
(t1 10000)
(t2 1000)



