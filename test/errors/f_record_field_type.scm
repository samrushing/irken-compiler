;; -*- Mode: Irken -*-

;; adding a string field to an int field.

;; expect-error: f_record_field_type.scm:9:9: error: type error

(include "lib/basis.scm")
(define r {a=1 b="x"})
(printn (+ r.a r.b))
