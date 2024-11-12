;; -*- Mode: Irken -*-

(require "lib/basis.scm")

(define s0 '(0 1 2 3 4 5))
(define s1 '(6 7 8 9 10 11))
(define e0 (:tuple s0 s1))
(define e1 (list (:tuple 0 6) (:tuple 1 7) (:tuple 2 8) (:tuple 3 9) (:tuple 4 10) (:tuple 5 11)))

(assert (magic=? (zip s0 s1) e1))
(assert (magic=? (unzip e1) (:tuple s0 s1)))

