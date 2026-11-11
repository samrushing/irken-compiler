;; -*- Mode: Irken -*-

;; a type error on a vector element.

;; expect-error: f_vector_type.scm:9:10: error: type error

(include "lib/basis.scm")
(define v #(1 2 3))
(printn (string-length v[0]))
