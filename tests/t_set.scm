;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/set.scm")

(define (test-set)
  (let ((s0 (make-set '() =))
	(s1 (make-set '() eq?))
	)
    (printn (s0.in 34))
    (s0.add 12)
    (s0.add 15)
    (s0.add 94)
    (s0.add 12)
    (printn (s0.in 27))
    (printn (s0.in 15))
    (printn (s0.in 12))
    (newline)
    (printn (s1.in 'x))
    (s1.add 'y)
    (s1.add 'a)
    (s1.add 'thing)
    (s1.add 'y)
    (printn (s1.in 'not))
    (printn (s1.in 'a))
    (printn (s1.in 'y))
    (newline)
    (printn (s0.get))
    (printn (s1.get))
    ))

(test-set)


    
