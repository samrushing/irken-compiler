(include "lib/core.scm")
;(include "lib/frb.scm")
(include "lib/pair.scm")
(include "lib/string.scm")
;(include "lib/vector.scm")
(include "lib/symbol.scm")

(define (thing)
  (%printn (cons 'symbol 'thingy))
  (%printn (string->symbol "thingy"))
  'thingy
  )

(%printn (thing))
