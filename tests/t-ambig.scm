;; -*- Mode: Irken -*-

(include "lib/core.scm")

(define r0 {a=1 b=2 m=3})
(define r1 {a=1 b=2 c=3 m=4})
(define r2 {a=1 b=2 c=3 d=4 m=5})

(printn r0)
(printn r1)
(printn r2)

(printn r0.m)
(printn r1.m)
(printn r2.m)
#u
