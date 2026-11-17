;; -*- Mode: Irken -*-

;; record patterns: `...` matches records with more fields (#34), a closed
;;  pattern matches a record with exactly its fields.

(include "lib/basis.scm")

(define field-a
  {a=x ...} -> x)

(define sum
  {a=x b=y} -> (+ x y))

(printn (field-a {a=1 b=2 c=3}))
(printn (field-a {a=4}))
(printn (sum {b=5 a=6}))
;; => 1
;; => 4
;; => 11
