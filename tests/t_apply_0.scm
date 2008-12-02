(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/vector.scm")

;; make sure application of randomly-referenced closures still works

(define (x a b)
  (%+ a b))

(define (y a b)
  (%- a b))

(define (z)
  (let ((funs (vector x y)))
    ((vector-ref funs 1) 10 5)))

(%printn (z))
