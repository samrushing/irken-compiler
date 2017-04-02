;; -*- Mode: Irken -*-

(include "lib/basis.scm")

(define (thing1)
  (printf "thing!\n"))

(define original-thing1 thing1)

(define (thing1-wrap)
  (printf "thing-wrap!\n")
  (original-thing1))

(set! thing1 thing1-wrap)

(thing1)
