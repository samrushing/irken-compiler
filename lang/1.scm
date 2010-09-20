;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")

(define length
    ()       -> 0
    (_ . tl) -> (+ 1 (length tl)))

(define length1
  (list:nil)       -> 0
  (list:cons _ tl) -> (+ 1 (length1 tl)))

(define length2
  ()       acc -> acc
  (_ . tl) acc -> (length2 tl (+ 1 acc))
  )

(define (length3 l)
  (length2 l 0))

(define (length4 l)
  (define recur
    ()       acc -> acc
    (_ . tl) acc -> (recur tl (+ 1 acc))
    )
  (recur l 0)
  )

(length4 '(1 2 3 4 5))

    