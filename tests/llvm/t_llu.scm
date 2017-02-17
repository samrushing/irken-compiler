;; -*- Mode: Irken -*-

(define (^llvm-mv r)
  (%call (DO ('a -> undefined)) r))

(^llvm-mv {x=3 y="testing" z=7})
