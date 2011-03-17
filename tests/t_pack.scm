;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")

(define (cmp-first a b)
  (= (first a) (first b)))

;; this verifies that the packing is 'stable'
(pack '((1 0) (1 1) (1 2) (2 0) (3 0) (3 1) (3 2) (4 0) (4 1) (2 1) (2 2) (2 3)) cmp-first)
