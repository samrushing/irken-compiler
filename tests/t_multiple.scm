;; -*- Mode: Irken -*-

(include "lib/basis.scm")

(define (divmod x n)
  (:tuple (/ x n) (mod x n)))

(let (((q r) (divmod 1000 3)))
  (printf "q= " (int q) " r= " (int r) "\n")
  )
