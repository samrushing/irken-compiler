;; -*- Mode: Irken -*-

;; assigning a string to an int variable.

;; expect-error: f_set_type.scm:9:2: error: type error

(include "lib/basis.scm")
(define x 1)
(set! x "a")
