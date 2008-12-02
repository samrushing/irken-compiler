(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")
(include "lib/symbol.scm")
(include "lib/vector.scm")

(define (thing)
  '(1 (4 5) 2 (3 4) 5))

(%printn (thing))
(%printn '(hello there #(this is a test) of the thing))
(%printn '#(1 2 3 4))
(%printn '#f)
(%printn ''''#f)

  
