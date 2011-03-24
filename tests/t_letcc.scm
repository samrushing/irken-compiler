;; -*- Mode:Irken -*-

(include "lib/core.scm")

(define (thing n)
  (let/cc exit
      (for-range
	  i 1000
	  (printn i)
	  (if (= i n)
	      (exit i))
	  )
    1001))

(define (test)
  (printn "before")
  (printn (thing 20))
  (printn "after"))

(test)

 

 