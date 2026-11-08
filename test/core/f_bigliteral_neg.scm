;; -*- Mode: Irken -*-

;; one below the smallest integer, -2^62.

;; expect-error: integer literal too big

(include "lib/basis.scm")

(printn -4611686018427387905)
