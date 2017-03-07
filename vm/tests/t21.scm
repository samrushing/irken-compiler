;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/frb.scm")

;; test tree/make macro

(let ((t (tree/make <
	  (1 #\t)
	  (2 #\f)
	  (3 #\l)
	  (4 #\a)
	  (5 #\b)
	  )))
  (print t)
  (tree/member t < 5)
  )
