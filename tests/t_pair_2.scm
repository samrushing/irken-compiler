(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")
(include "lib/vector.scm")
(include "lib/frb.scm")
(include "lib/symbol.scm")

(define (thing)
  #((1 . 2) (3 . 4)))

(let ((x (thing)))
  (%printn (vector-ref x 0))
  (%printn (vector-ref x 1))
  )

