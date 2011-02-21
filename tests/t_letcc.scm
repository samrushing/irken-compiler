;; -*- Mode:Irken -*-

(include "lib/core.scm")

(define (thing n)
  (let/cc exit
	 (let loop ((i 0))
	   (printn i)
	   (cond ((= i n) (exit i));; (throw exit i))
		 ((> i 10) 3141)
		 (else (loop (+ i 1))))
	   #t)))

(define (test)
  (printn "before")
  (thing 20)
  (printn "after"))

(test)

 

 