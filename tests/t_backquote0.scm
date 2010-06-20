;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")
(include "lib/frb.scm")
(include "lib/symbol.scm")

(datatype list
  (:nil)
  (:cons 'a (list 'a)))

(define animal0 'fox)
(define animal1 'dog)

`(the quick brown ,animal0 jumped over the lazy ,animal1)

  
  