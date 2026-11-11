;; -*- Mode: Irken -*-

;; a record literal that names a field twice (#42).

;; expect-error: f_record_dup_field.scm:9:20: error: field `a` appears twice in this record

(include "lib/basis.scm")

(define r {a=1 b=2 a=3})
(printn r.a)
