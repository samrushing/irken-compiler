;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")

;; test records in datatypes

;; note that the '...' is missing, meaning that the
;;   record type is closed.  test and test2 attempts to
;;   throw other stuff in should thus fail.

(datatype thing
  (:t {x=int y=char})
  )

(define (test)
  (thing:t {x=3 y=#\b z=9})
  )

(define (test2)
  (thing:t {x=4 y=#\c z=#\a a=#t b=#f}))

(printn (test))
(printn (test2))



