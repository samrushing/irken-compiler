;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/frb.scm")

;; test tree/make macro

(let ((t (tree/make <
	  (1 "time")
	  (2 "flies")
	  (3 "like")
	  (4 "a")
	  (5 "banana")
	  )))
  (printn t)
  (tree/member t < 5)
  )



 
    