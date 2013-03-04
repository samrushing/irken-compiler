;; -*- Mode: Irken -*- 

(include "lib/basis.scm")
(include "lib/os.scm")

;;; Note: if this test is failing you may need to disable ASLR on your
;;;   operating system.  Otherwise the addresses of continuations/functions/etc
;;;   will not be identical between runs and the loading of image dumps will fail.

(define (thing n)
  (printn "howdy!") 5)

(define (go)
  (let loop ((n 100))
    (print n) (print-string " ")
    (cond ((= n 0) 9)
	  ((= n 50)
	   (callcc (lambda (k) (dump "test.image" k)))
	   (loop (- n 1)))
	  (else (loop (- n 1))))))

;; invoke without an argument to dump the image,
;; with an argument to load it and run it.
(if (> sys.argc 1)
    (throw (load "test.image") 0)
    (go))


