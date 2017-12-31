;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")
(include "demo/bignum.scm")
(include "lib/mtwist.scm")

;; find a value for KARATSUBA-CUTOFF
;; final result: ~25 digits.
;; https://docs.google.com/spreadsheets/d/e/2PACX-1vQgVas0tT59VVqsIKAf4bA5ajUzO1n7iM1s0n9OzYycrG1M_eeS-IsXpzP7BsNQNeo7naXoPGUL1bZ-/pubchart?oid=102760542&format=interactive

(define (ticks)
  (%%cexp (-> int) "(pxll_int)rdtsc()"))

(defmacro timeit
  (timeit body ...)
  -> (let (($t0 (ticks)))
       body ...
       (- (ticks) $t0)))

;; first, generate a range of random values that we
;;  can use to construct bignums of various sizes.

(define random-values
  (let ((v (make-vector 1000 0))
        (rngdig (generate-random-bits big/bits 3141)))
    (for-range i 1000
      (when-maybe r (rngdig)
        (set! v[i] r)))
    v))

;; do timings on variously-sized bignums

(define (mul-size n)
  (let ((n0 (big:pos (vslice random-values 0 n)))
        (n1 (big:pos (vslice random-values n (+ n n)))))
    (big-mul n0 n1)))

;; I ran this with various values for the cutoff, rather than mutating the global cutoff
;; value from here - in case making it a mutable global (rather than a constant) affected
;; the results.

;; this tests values between 2 to 500 digits, writes CSV to stdout.

(for-range* i 2 50
  (printf "\"" (int (* i 10)) "\",\"" (int (timeit (mul-size (* 10 i)))) "\"\n")
  )
