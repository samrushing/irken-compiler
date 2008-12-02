(define (test n) n)
(define (zord n) (%print 99) (%+ n 1))
(test (zord 5))
(include "tests/t4.scm")
