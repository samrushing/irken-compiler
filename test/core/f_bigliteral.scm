;; -*- Mode: Irken -*-

;; an integer literal that doesn't fit (2^62 here) is an error, rather
;;  than silently wrapping around.

;; expect-error: integer literal too big

(include "lib/basis.scm")

(printn 4611686018427387904)
