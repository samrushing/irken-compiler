
(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")

(define (thing n)
  (printn "howdy!") 5)

(define (go)
  (let loop ((n 100))
    (print n) (print-string " ")
    (cond ((= n 0) 9)
	  ((= n 50)
	   (call/cc (lambda (k) (dump "test.image" k)))
	   (loop (- n 1)))
	  (else (loop (- n 1))))))

;; invoke without an argument to dump the image,
;; with an argument to load it and run it.
(if (> (sys:argc) 1)
    ((load "test.image") 0)
    (go))


