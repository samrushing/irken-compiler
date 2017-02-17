;; -*- Mode: Irken -*-

(datatype X
  (:r int int)
  (:g int)
  (:b)
  )

(define (^llvm-x)
  (X:r 19 27)
  )

(^llvm-x)
