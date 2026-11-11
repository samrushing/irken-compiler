;; -*- Mode: Irken -*-

;; a record pattern that names a field twice.

;; expect-error: f_record_pattern_dup_field.scm:10:8: error: field `a` appears twice in this record pattern

(include "lib/basis.scm")

(define g
  {a=x a=y} -> x)
(printn (g {a=1}))
