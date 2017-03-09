;; -*- Mode: Irken -*-

(include "lib/core.scm")

(define thing #(0 1 2 3 4 5))
(set! thing[2] 19)
(print thing[4])
(print thing[2])
(print (%make-vector #f 10 23))


