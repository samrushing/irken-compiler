;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")

(define thing
  "testing" -> #t
  "blurble" -> #t
  _         -> #f
  )

(printn (thing "blurble"))
(printn (thing "not really"))

