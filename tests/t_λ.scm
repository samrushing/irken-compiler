;; -*- Mode: Irken -*-

(include "lib/core.scm")

(define add
   (λ (x)
     (λ (y)
       (+ x y))))

((add 3) 4)
