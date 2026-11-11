;; -*- Mode: Irken -*-

;; the same for hex: a 64-bit mask doesn't fit in an irken integer.

;; expect-error: f_bigliteral_hex.scm:9:9: error: integer literal too big

(include "lib/basis.scm")

(printn #xffffffffffffffff)
