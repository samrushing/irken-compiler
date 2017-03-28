
(include "lib/core.scm")

(define (thing1 a b c) (+ a 1))
(define (thing2 a b c) (+ b 1))
(define (thing3 a b c) (+ c 1))

(print (thing1 10 11 12))
(print (thing2 10 11 12))
(print (thing3 10 11 12))
