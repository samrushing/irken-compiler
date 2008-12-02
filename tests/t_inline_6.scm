
(include "lib/core.scm")

;; inliner should collapse these two lets...
;; should return 68
(define (thing)
  (let ((x
	 (let ((y 10)
	       (a
		(let ((c 10))
		  (+ c c)))
	       (b 3))
	   (+ y (+ a b))))
	(z (+ x 2)))
    (+ x z)))

(thing)
	 