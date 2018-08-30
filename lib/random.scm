;; -*- Mode: Irken -*-

(require-ffi 'posix)

(define (random)
  (posix/random))

(define (srandom n)
  (posix/srandom n))
