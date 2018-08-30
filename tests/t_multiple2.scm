;; -*- Mode: Irken -*-

(include "lib/core.scm")

(let (((q0 r0) (divmod 1000 3))
      ((q1 r1) (divmod 555 9))
      (n 34)
      ((q2 r2) (divmod 3141 7)))
  (printn q0)
  (printn r0)
  (printn q1)
  (printn r1)
  (printn n)
  (printn q2)
  (printn r2)
  )
