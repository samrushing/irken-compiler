;; -*- Mode: Irken -*-

;; reading a field the record doesn't have is a type error.

;; expect-error: f_record_missing.scm:10:16: error: type error

(include "lib/basis.scm")

(define (get-x r) r.x)
(printn (get-x {y=1}))
