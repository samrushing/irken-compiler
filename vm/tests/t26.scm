;; -*- Mode: Irken -*-

(include "lib/core.scm")

(define thing1 {a=0 x=1 y=2})
(define thing2 {x=3 y=4 z=7})

(define (fun1 p)
  (+ p.x 10)
  )

(print (fun1 thing1))
(print (fun1 thing2))

  
