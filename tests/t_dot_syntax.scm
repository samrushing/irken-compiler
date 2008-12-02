
(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")
(include "lib/vector.scm")
(include "lib/frb.scm")
(include "lib/symbol.scm")


(define (fake-object msg)
  (print-string "msg=")
  (%printn msg)
  fake-object)

(define (thing)
  (let ((x 'testing))
    (fake-object.b.c.d 23 24)))

(thing)
