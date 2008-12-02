(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")

(define (thing exit)
  (let loop ((n 50))
    (%print n) (print-string " ")
    (cond ((= n 0)
	   (%printn "done!"))
	  ((= n 20)
	   (^call/cc (lambda (p) (dump "test.image" p) (exit "exiting!")))
	   (loop (- n 1)))
	  (else
	   (loop (- n 1))))))

(%printn (sys.argv 0))
(if (and (> (sys.argc) 1) (string-=? (sys.argv 1) "-d"))
    (^call/cc (lambda (exit) (thing exit)))
    ((load "test.image")))
