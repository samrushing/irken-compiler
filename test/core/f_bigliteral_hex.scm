;; -*- Mode: Irken -*-

;; the same for hex: a 64-bit mask doesn't fit in an irken integer.

;; expect-error: integer literal too big

(include "lib/basis.scm")

(printn #xffffffffffffffff)
