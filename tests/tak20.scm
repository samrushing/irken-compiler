;; -*- Mode: Irken -*-

;; Tak, the Hideous New Girl!

(include "lib/core.scm")

(define (tak x y z)
  (if (>= y x)
      z
      (tak (tak (- x 1) y z)
	   (tak (- y 1) z x)
	   (tak (- z 1) x y))))

(let loop ((n 20))
  (let ((r (tak 18 12 6)))
    (if (= 0 n)
	r
	(loop (- n 1)))))
