;; -*- Mode: Irken -*-

;; one below the smallest integer, -2^62.

;; expect-error: f_bigliteral_neg.scm:9:9: error: integer literal too big

(include "lib/basis.scm")

(printn -4611686018427387905)
