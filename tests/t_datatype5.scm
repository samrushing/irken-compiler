;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")

;; test records in datatypes

(datatype thing
  (:a {x=int y=char})
  (:b {x=bool y=string})
  )

(datatype thing2
  (:t {x=int y={a=int b=int c=int}}))

(define (test)
  (thing:a {x=3 y=#\A})
  )

(printn (test))
(printn (thing2:t {x=5 y={a=3 b=3 c=6}}))


