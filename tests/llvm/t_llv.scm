;; -*- Mode: Irken -*-

(define (^llvm-mv r)
  (%%ffi DO ('a -> undefined) r)
  r)

(^llvm-mv {x=3 y="testing" z=7})
