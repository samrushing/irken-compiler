(include "lib/core.scm")

(define (thing x)
  (if (eq? x -1)
      "yes"
      "no"))

(thing (- 0 1))
