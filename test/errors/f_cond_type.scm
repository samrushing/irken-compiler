;; -*- Mode: Irken -*-

;; cond arms of different types: at the cond (a macro).

;; expect-error: f_cond_type.scm:9:3: error: type error

(include "lib/basis.scm")
(define (f x)
  (cond ((= x 1) "one")
        (else 2)))
(printn (f 1))
