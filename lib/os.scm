;; -*- Mode: Irken -*-

(define (system cmd)
  (%%cexp (string -> int) "system (%0)" cmd))
  