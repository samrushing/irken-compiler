;; -*- Mode: Irken -*-

;; a polymorphic variant (or record) whose value nothing uses: the compiler
;;  gave it the 'dead' target register (-1), and emitted `O r-1 = ...` (C)
;;  or wrote to a bogus register (native).  its args must still run.

(include "lib/basis.scm")

(define (f)
  (:dead0)
  (:dead1 (begin (printn 'arg) 1))
  {a=(begin (printn 'field) 2) b=3}
  0)

(printn (f))
;; => arg
;; => field
;; => 0
