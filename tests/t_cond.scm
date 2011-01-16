;; -*- Mode: Irken -*-

(include "lib/core.scm")

(define (thing x)
  (cond ((= x 0) 1)
	((= x 1) 2)
	(else 3)))

(thing 5)
