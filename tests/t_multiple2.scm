;; -*- Mode: Irken -*-

(include "lib/core.scm")

(define (divmod x n)
  (:tuple (/ x n) (remainder x n)))

(let (((q0 r0) (divmod 1000 3))
      ((q1 r1) (divmod 555 9)))
  (printn q0)
  (printn r0)  
  (printn q1)
  (printn r1)
  )
