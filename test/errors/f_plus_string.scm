;; -*- Mode: Irken -*-

;; adding a string: at the `+`, not inside lib/core.scm.

;; at the `+` (C backend) or at the "x" (native backends): only the line
;;  is checked.

;; expect-error: f_plus_string.scm:11:

(include "lib/basis.scm")
(printn (+ 1 "x"))
