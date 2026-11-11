;; -*- Mode: Irken -*-

;; an unclosed paren: at the paren that is never closed.

;; expect-error: f_unbalanced.scm:8:1: error: this list is never closed

(include "lib/basis.scm")
(define (f x)
  (+ x 1)
