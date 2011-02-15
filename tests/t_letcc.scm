;; -*- Mode:Irken -*-

(include "lib/core.scm")

(define (test)
  (printn "before")
  (printn
   (let/cc
    exit
    (let loop ((n 100))
      (printn n)
      (if (= n 80)
	  (exit n)
	  (loop (- n 1)))
      3141)))
  (printn "after"))

(test)

 

 