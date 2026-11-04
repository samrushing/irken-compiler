;; -*- Mode: Irken -*-

;; records of closures: the "object" style doom uses for sockets.

(include "lib/basis.scm")

(define (make-counter start)
  (let ((n start))
    (define (incr) (set! n (+ n 1)) n)
    (define (get) n)
    {inc=incr get=get name="counter"}))

(define c1 (make-counter 0))
(define c2 (make-counter 100))
(c1.inc) (c1.inc) (c2.inc)
(printn (list (c1.get) (c2.get)))
(printn c1.name)
;; => (2 101)
;; => "counter"

;; something that takes any "object" with an inc method
(define (inc-twice obj) (obj.inc) (obj.inc))
(define logger
  (let ((lines '()))
    {inc=(lambda () (set! lines (list:cons "x" lines)) (length lines))
     lines=(lambda () lines)}))
(printn (list (inc-twice c1) (inc-twice logger)))
(printn (logger.lines))
;; => (4 2)
;; => ("x" "x")
