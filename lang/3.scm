;; -*- Mode: Irken -*-

(include "lib/core.scm")

(define v0 (:pair 3 #t))
(define v1 (:thingy 12))

(define (fun x)
  (vcase x
    ((:pair n b) n)
    ((:thingy n) n)
    ))

(printn v0)
(printn (fun v0))
(printn (fun v1))
