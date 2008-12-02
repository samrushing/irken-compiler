(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")
(include "lib/symbol.scm")

(define (thing n)
  (%printn '(x y z))
  (case n
    ((a b c) (%print "one two three"))
    ((d e) (%print "four") (%print "five"))
    ((f) (%print "six"))
    (else (%print "else"))))

(thing 'b)
(thing 'c)
(thing 'e)
(thing 'f)
(thing 23)
