(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")

(define (thing)
  (string-join "hello " "there " "how " "are " "you?"))

(define (thing2)
  (sum-string-lengths "hello " "there " "how " "are " "you?"))

;(%printn (thing))
(%printn (thing2))
