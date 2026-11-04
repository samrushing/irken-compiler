;; -*- Mode: Irken -*-


(require "lib/basis.scm")

(define (thing a b)
  (printf (int a) " / " (int b) " => " (int (/ a b)) "\n")
  )

(thing 10 3)
(thing 10 0)

