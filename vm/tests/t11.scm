
(include "lib/core.scm")

(define calls 0)

(define (tak x y z)
  (set! calls (+ 1 calls))
  (if (>= y x)
      z
      (tak (tak (- x 1) y z)
	   (tak (- y 1) z x)
	   (tak (- z 1) x y))))

(let loop ((n 20))
  (let ((r (tak 18 12 6)))
    (if (= n 0)
        r
	(loop (- n 1)))))

calls

