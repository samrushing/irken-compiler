;; -*- Mode: Irken -*-

;; a constructor of a datatype that does not exist.

;; expect-error: f_unknown_datatype.scm:9:10: error: unknown datatype `thing`

(include "lib/basis.scm")

(printn (thing:one 1))
