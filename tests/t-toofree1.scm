;; -*- Mode: Irken -*-


(require "lib/basis.scm")

(define thing
  (1 2 3 d 5 f 7) -> (+ d f)
  () -> 0
  _ -> 12
  )

(thing '(1 2 3 4 5 6 7))
