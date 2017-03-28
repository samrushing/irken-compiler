;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/frb.scm")

;; test tree/make macro

(let ((t (tree/make int-cmp
	  (1 "time")
	  (2 "flies")
	  (3 "like")
	  (4 "a")
	  (5 "banana")
	  )))
  (printn t)
  (tree/member t int-cmp 5)
  )
