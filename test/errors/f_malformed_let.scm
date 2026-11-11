;; -*- Mode: Irken -*-

;; a malformed `let` binding: at the `let`, not inside lib/derived.scm.

;; expect-error: f_malformed_let.scm:9:3: error: malformed binding in LET-SPLAT: (a)

(include "lib/basis.scm")
(define (f x)
  (let ((a)) a))
(f 1)
