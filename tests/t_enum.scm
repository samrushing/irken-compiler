
(include "lib/core.scm")

(define (print-imm x)
  (%%cexp ('a -> int) "fprintf (stderr, \"code=%%p\\n\", (object*)%s)" x))

(datatype foo
  (:one)
  (:two)
  (:three)
  )

(datatype glork
  (:a)
  (:b)
  )

(define bar
  (foo:one) -> 1
  (foo:two) -> 2
  (foo:three) -> 3
  )

(printn (bar (foo:one)))
(printn (bar (foo:two)))
(printn (bar (foo:three)))
(print-imm (foo:one))
(print-imm (foo:two))
(print-imm (foo:three))
;(print-imm #t)
;(print-imm #f)
;(print-imm (glork:a))
;(print-imm (glork:b))

