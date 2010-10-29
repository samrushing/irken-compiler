;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")

(define (thing n)
  (if (zero? n)
      (maybe:no)
      (maybe:yes { sub=(thing (- n 1)) })))

(thing 5)
