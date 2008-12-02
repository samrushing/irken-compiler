
(include "lib/core.scm")

;; based on the code from here:
;;   http://en.wikipedia.org/wiki/Call-with-current-continuation

(define (make-gen)
  (define (generator)
    (call/cc control-state))
  (define (control-state return)
    (let loop ((n 0))
      (set! return
	    (call/cc (lambda (resume-here)
			(set! control-state resume-here)
			(return n))))
      (loop (%+ n 1))))
  generator)

(let ((g (make-gen)))
  (%printn g)
  (%printn (g))
  (%printn (g))
  (%printn (g))
  (%printn (g))
  (%printn (g))
  )
   

