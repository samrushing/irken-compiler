;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/random.scm")

(srandom 314159)

(define nrandom
  acc 0 -> acc
  acc n -> (nrandom
	    (list:cons (mod (random) 100000) acc)
	    (- n 1)))

(let ((rl (nrandom '() 100)))
  (printn rl)
  (printn (sort < rl)))
