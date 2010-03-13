
(include "lib/core.scm")

(define (thingy sym)
  (case sym
    ((thing1 thing2 thing3) 12)
    ((thing4) 13)
    (else 14)))

(thingy 'dweezil)

   