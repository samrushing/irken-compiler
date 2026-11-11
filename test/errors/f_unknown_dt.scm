;; -*- Mode: Irken -*-

;; a malformed formal parameter.

;; expect-error: f_unknown_dt.scm:8:12: error: malformed formal: (x:nope)

(include "lib/basis.scm")
(define (f (x : nope)) x)
