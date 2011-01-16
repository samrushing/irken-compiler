
(include "lib/core.scm")
(include "lib/pair.scm")

(define thing
  ('a 'b 'c) -> #t
  _ -> #f
  )

(printn (thing '(a b c)))
(printn (thing '(c b a)))
