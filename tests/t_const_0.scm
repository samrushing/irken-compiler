(include "lib/core.scm")
(include "lib/pair.scm")

(define (thing)
  (cons "a car" "a cdr"))

(%printn (thing))
