;; -*- Mode: Irken -*-

;; integer constants that need a full 64-bit word: wider than 32 bits,
;;  with zero 16-bit pieces, negative, and "wide" (|n| >= 2^61, whose
;;  word the compiler writes out in hex).  the arm backend builds these
;;  with movz/movk (self/arm.scm, emit-wide-immediate); this checks it
;;  agrees with the other backends.

(include "lib/basis.scm")

(printn #xffffffff)
(printn #x100000000)
(printn #x100000001)
(printn #x0000ffff0000ffff)
(printn #x1234567890abcdef)
(printn -4294967296)
(printn -4294967297)
(printn -1311768467294899695)
(printn 2305843009213693952)
(printn -2305843009213693953)
(printn 4611686018427387903)
(printn -4611686018427387904)

;; => 4294967295
;; => 4294967296
;; => 4294967297
;; => 281470681808895
;; => 1311768467294899695
;; => -4294967296
;; => -4294967297
;; => -1311768467294899695
;; => 2305843009213693952
;; => -2305843009213693953
;; => 4611686018427387903
;; => -4611686018427387904

;; and in arithmetic: masks, as (logand x #xffffffff) uses.
(printn (logand 12345678901234 #xffffffff))
(printn (+ #xffffffff 1))
(printn (- 2305843009213693952 1))
(printn (logxor #x0000ffff0000ffff #x00ff00ff00ff00ff))
;; => 1942892530
;; => 4294967296
;; => 2305843009213693951
;; => 72056494543077120
