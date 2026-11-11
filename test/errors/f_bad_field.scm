;; -*- Mode: Irken -*-

;; reading a field the record does not have.

;; expect-error: f_bad_field.scm:9:9: error: type error

(include "lib/basis.scm")
(define r {a=1 b=2})
(printn r.c)
