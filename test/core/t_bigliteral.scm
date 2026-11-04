;; -*- Mode: Irken -*-

;; integer constants beyond +/-2^61.  fixnums go up to 2^62-1 at run time
;;  (see t_arith), but the compiler tags a constant with its own fixnum
;;  arithmetic, (n << 1) | 1, which overflows for |n| >= 2^61.  folded
;;  constants hit this too.

;; xfail: c arm amd rv -- compile-time constants >= 2^61 wrap around

(include "lib/basis.scm")

(printn 2305843009213693952)
(printn 4611686018427387903)
(printn (<< 1 61))
(printn (+ 2305843009213693951 1))
;; => 2305843009213693952
;; => 4611686018427387903
;; => 2305843009213693952
;; => 2305843009213693952
