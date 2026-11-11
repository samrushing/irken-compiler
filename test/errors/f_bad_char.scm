;; -*- Mode: Irken -*-

;; a character literal that does not exist: at the literal.

;; expect-error: f_bad_char.scm:8:9: error: bad character literal: "zzz"

(include "lib/basis.scm")
(printn #\zzz)
