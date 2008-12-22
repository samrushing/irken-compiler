(include "lib/core.scm")
(include "lib/string.scm")

(define (thingy)
  (printn "howdy there!\n")
  ;; thunks must return an integer
  0
  )

(printn (sys:argv 0))
(if (and (> (sys:argc) 1) (string-=? (sys:argv 1) "-d"))
    (dump "test.img" thingy)
    ((load "test.img")))

