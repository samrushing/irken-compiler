;; -*- Mode: Irken -*-

(define (^llvm-x n)
  (%%ffi putchar (int -> undefined) n)
  n
  )

(^llvm-x 65)

