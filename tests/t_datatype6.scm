;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")

;; declare a new datatype <thing> with only one alternative <t>
;;   which consists of an open record type... i.e., a record that
;;   may contain other unknown fields.
(datatype thing
  (:t {x=int y=char ...})
  )

(define (test1)
  (thing:t {x=3 y=#\b z=9})
  )

(define (test2)
  (thing:t {x=4 y=#\c z=#\a a=#t b=#f}))

;; make sure it still works with *no* extra fields
(define (test3)
  (thing:t {x=4 y=#\a}))

(printn (test1))
(printn (test2))
(printn (test3))



