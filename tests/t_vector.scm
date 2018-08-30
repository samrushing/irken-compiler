
(include "lib/core.scm")
(include "lib/pair.scm")

(define (thing v) v[0])

(printn (thing #(1 2 3)))
(printn (vector-length #(1 2 3 4 5)))
(printn (vector-length #()))
(printn (vector-length (make-vector 0 0)))
(printn (vector->list #(1 2 3 4 5)))
(printn (list->vector (vector->list #(1 2 3 4 5))))
