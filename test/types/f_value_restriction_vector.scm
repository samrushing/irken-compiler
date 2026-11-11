;; -*- Mode: Irken -*-

;; the value restriction: the same, with a vector.

;; expect-error: error: type error

(include "lib/basis.scm")

(define v (make-vector 1 (list:nil)))
(set! v[0] (list:cons 1 (list:nil)))
(define (f) (match v[0] with (list:cons s _) -> (string-length s) _ -> 0))
(printn (f))
