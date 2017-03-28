(include "lib/core.scm")

(define (plus a b)
  (+ a b))

(plus 100 (plus 1 (plus 2 (plus 3 4))))

