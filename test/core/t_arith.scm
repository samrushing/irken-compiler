;; -*- Mode: Irken -*-

;; integer arithmetic.  `/` and `mod` are Euclidean: the remainder is
;;  never negative.  remainder truncates, modulo floors.

(include "lib/basis.scm")

(printn (+ 2 3))
(printn (- 2 3))
(printn (* -4 5))
(printn (* 1000000007 1000000009))
;; => 5
;; => -1
;; => -20
;; => 1000000016000000063

(printn (list (/ 8 3) (/ -8 3) (/ 8 -3) (/ -8 -3)))
(printn (list (mod 8 3) (mod -8 3) (mod 8 -3) (mod -8 -3)))
(printn (list (remainder -8 3) (remainder 8 -3)))
(printn (list (modulo -8 3) (modulo 8 -3)))
;; => (2 -3 -2 3)
;; => (2 1 2 1)
;; => (-2 2)
;; => (1 -1)

(let (((q r) (divmod -7 2)))
  (printn (list q r)))
;; => (-4 1)

(printn (list (<< 1 40) (>> -16 2) (>> 1024 3)))
(printn (list (logand #xff0 #x0ff) (logior 5 10) (logxor 15 5) (lognot 0)))
;; => (1099511627776 -4 128)
;; => (240 15 10 -1)

(printn (list (min 3 -2) (max 3 -2) (abs -9) (abs 9) (pow 2 10)))
(printn (list (odd? 7) (even? 7) (zero? 0) (>0 -1) (<0 -1)))
;; => (-2 3 9 9 1024)
;; => (#t #f #t #f #t)

;; the largest fixnum is 2^62-1 (one bit for the tag), the smallest -2^62.
;;  (computed at run time: see t_bigliteral for constants this size.)
(define (shl a b) (<< a b))
(define big (+ (shl 1 61) (- (shl 1 61) 1)))
(printn big)
(printn (- -1 big))
;; => 4611686018427387903
;; => -4611686018427387904

(define (fact n)
  (if (= n 0) 1 (* n (fact (- n 1)))))
(printn (fact 20))
;; => 2432902008176640000
