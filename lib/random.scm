;; -*- Mode: Irken -*-

;; XXX needs to use libc ffi.

(define (random)
  (%%cexp (-> int) "random()"))

(define (srandom n)
  (%%cexp (int -> undefined) "srandom (%0)" n))
