(include "lib/core.scm")
(include "lib/pair.scm")

(define (thing n)
  (case n
    ((1 2 3) (%print "one two three"))
    ((4 5) (%print "four") (%print "five"))
    ((6) (%print "six"))
    (else (%print "else"))))

(thing 2)
(thing 5)
(thing 4)
(thing 6)
(thing 23)
