(include "lib/core.scm")

(define thing
  #\A -> 0
  #\B -> 1
  #\C -> 2
  _   -> 3
  )

(printn (thing #\A))
(printn (thing #\B))
(printn (thing #\C))
(printn (thing #\D))
