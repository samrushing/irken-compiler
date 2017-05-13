;; -*- Mode: Irken -*-

(include "lib/core.scm")

(typealias ralias {a=int b='a})

(datatype thing
  (:one (ralias int))
  (:two (ralias char))
  )

(define thing1 (thing:one {a=1 b=3}))
(define thing2 (thing:two {a=2 b=#\A}))

(printn thing1)
(printn thing2)

