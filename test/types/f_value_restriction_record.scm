;; -*- Mode: Irken -*-

;; the value restriction: a record is mutable, so a top-level binding to one
;;  is not polymorphic.  without the restriction, this stored an int and
;;  read it back as a string (and crashed).

;; expect-error: error: type error

(include "lib/basis.scm")

(define r {v=(list:nil)})
(set! r.v (list:cons 1 (list:nil)))
(define (f) (match r.v with (list:cons s _) -> (string-length s) _ -> 0))
(printn (f))
