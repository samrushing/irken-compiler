;; -*- Mode: Irken -*-


(require "lib/core.scm")

(define (many a b c d e f g h)
  (%asm-c-call
   ("amd_extra_test"
    (int int int int int int int int -> int))
   a b c d e f g h)
  )

(many 1 2 3 4 5 6 7 8)
