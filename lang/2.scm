;; -*- Mode: Irken -*-

(include "lib/core.scm")

(define my-record {f0=#t f1="stringy" f2=1234})

(define (fun r)
  (+ r.f2 34))

(printn my-record.f0)
(printn my-record.f1)
(printn fun)
