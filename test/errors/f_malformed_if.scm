;; -*- Mode: Irken -*-

;; an `if` with nothing in it.

;; expect-error: f_malformed_if.scm:9:4: error: malformed <if>: (if)

(include "lib/basis.scm")
(define (f x)
  (if))
(f 1)
