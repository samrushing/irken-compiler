;; -*- Mode: Irken -*-

;; getcc at the top level of a program crashes the compiler
;;  (Runtime Error, halting: "car").

;; xfail: c b arm amd rv -- the compiler dies on a top-level (getcc)

(include "lib/basis.scm")

(define saved (getcc))
(define counter 0)
(set! counter (+ counter 1))
(when (< counter 3)
  (putcc saved #u))
(printn counter)
;; => 3
