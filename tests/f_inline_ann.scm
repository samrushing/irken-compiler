;; -*- Mode: Irken -*-

(require "lib/core.scm")

;; exercise the reporting of errors with accurate
;;  inlined src information.

(define (a x y)
  (+ x y)
  )

(define (b x y)
  (* (a x y) (a x y))
  )

(define (c x y)
  (- 100 (b x y)))

(c 19 #f)


