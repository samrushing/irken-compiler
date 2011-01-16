;; -*- Mode: Irken -*-

(include "lib/core.scm")

(define thing1
  #\newline -> 0
  #\space   -> 1
  #\tab	    -> 2
  _	    -> 3
  )

(datatype color
  (:red)
  (:green)
  (:blue)
  )

(define thing2
  (color:red)	-> 0
  (color:green) -> 1
  (color:blue)	-> 2
  )

(printn (thing1 #\space))
(printn (thing2 (color:blue)))


