;; -*- Mode: Irken -*-

(require "lib/basis.scm")

(define (t0)

  (let ((t (tree/make int-cmp
             (1 "time")
             (2 "flies")
             (3 "like")
             (4 "a")
             (5 "banana")
             )))
    (printn t)
    (tree/dump 0 (lambda (k v d) (printf (repeat d "  ") (int k) " " (string v) "\n")) t)
    (tree/member t int-cmp 5)
    (let ((t1 (tree/delete t int-cmp 4)))
      (printn t1))
    ))

(include "lib/random.scm")

(define (t1 n)

  (define (pprint t)
    (tree/dump 0 (lambda (k v d) (printf (lpad 5 (int v)) " " (repeat d "  ") (int k) "\n")) t)
    )

  (let ((t (tree:empty))
        (keys0 '()))
    (srandom 3141596)
    (for-range i n
       (let ((v (random)))
         (tree/insert! t int-cmp v i)
         (push! keys0 v)
         ))
    (assert (tree/verify t))
    (let ((t2 t))
      (for-each
       (lambda (k)
         (set! t2 (tree/delete t2 int-cmp k))
	 (assert (tree/verify t2))
         )
       keys0))
    (printn (tree/min t))
    (printn (tree/max t))
    (let ((sorted-keys (sort < keys0))
	  (min-key (first sorted-keys))
	  (max-key (last sorted-keys))
          ((k0 v0) (tree/min t))
          ((k1 v1) (tree/max t)))
      (assert (= k0 min-key))
      (assert (= k1 max-key))
      (printn min-key)
      (printn max-key)
      )
    ))

(define (t2 n)
  (let ((t (tree:empty)))
    (srandom 3141596)
    (for-range i n
       (let ((v (random)))
         (set! t (tree/insert t int-cmp v i))
         ))
    (let ((bh (tree/black-height t 0)))
      (printf "black-height: " (int bh) "\n")
      (printn (tree/verify t)))
    ))

(t0)
(t1 10000)
(t2 1000)
