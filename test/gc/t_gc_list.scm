;; -*- Mode: Irken -*-

;; enough allocation to force many collections, with live data that must
;;  survive them intact.

(include "lib/basis.scm")

(define keep (range 1000))

(define (churn n)
  ;; build and drop a lot of short lists
  (let loop ((i 0) (total 0))
    (if (= i n)
        total
        (loop (+ i 1) (+ total (length (range 100)))))))

(printn (churn 100000))
;; => 10000000

;; the long-lived list is still intact
(printn (list (length keep) (fold binary+ 0 keep) (nth keep 999)))
;; => (1000 499500 999)

;; a growing structure held across collections
(define (build n)
  (let loop ((i 0) (acc '()))
    (if (= i n)
        acc
        (loop (+ i 1) (list:cons {i=i sq=(* i i) s=(int->string i)} acc)))))
(let ((big (build 200000)))
  (printn (length big))
  (printn (fold (lambda (r acc) (+ acc r.sq)) 0 big))
  (let ((r (first big)))
    (printn r.s)))
;; => 200000
;; => 2666646666700000
;; => "199999"
