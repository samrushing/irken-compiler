;; -*- Mode: Irken -*-

(define (popcount n)
  (%backend rv
    (%rv
     ((int -> int)
      (untag (a 0))
      (cpop trg (a 0))
      (tag trg))
     n))
  )
