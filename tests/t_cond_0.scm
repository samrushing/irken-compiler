

(define (test0 x)
  (cond ((%gt? x 7)
	 (set! x 3)
	 (%+ x 10))
	((%gt? x 3)
	 (set! x 9)
	 (%+ x 2))
	(else
	 (set! x 100)
	 (%+ x 1)))
   )

(%printn (test0 10))
(%printn (test0 5))
(%printn (test0 2))

(define (test1)
  (cond (else 1 2 3)))

(%printn (test1))

(define (test2 x)
  (cond ((if (%eq? x 5) 99 #f) => (lambda (x) (%+ x 5)))
	(else 23)))

(%printn (test2 5))
(%printn (test2 0))
	 
;; testing names with dots
(let ((nt #f))
  (define (name.test x)
    (%printn "hello there")
    (%printn "say what?")
    (%+ x 1))
  (set! nt name.test)
  (nt 13)
  (name.test 10)
  )



    