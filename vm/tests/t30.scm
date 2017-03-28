;; -*- Mode: Irken -*-

(include "lib/core.scm")

;; test op_rset.

(define (^fun1 p)
  (set! p.y 1001)
  (+ p.x 10)
  )

(let ((thing1 {a=0 x=1 y=2 b="what?"})
      (thing2 {x=3 y=4 z=7}))
  (set! thing1.x 27)
  (set! thing2.x 19)
  (print (^fun1 thing1))
  (print (^fun1 thing2))
  (print thing1)
  (print thing2)
  )


  
