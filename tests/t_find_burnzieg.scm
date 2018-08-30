;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")
(include "demo/bignum.scm")
(include "lib/mtwist.scm")

;; find a value for BURNZIEG-CUTOFF
;; final result: 10.

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
  (let ((v (make-vector 2000 0))
        (rngdig (generate-random-bits big/bits 5859)))
    (for-range i 1000
      (when-maybe r (rngdig)
        (set! v[i] r)))
    v))

;; do timings on variously-sized bignums

(define (div-size n)
  (let ((n2 (* n 2))
        (n0 (big:pos (vslice random-values 0 n2)))
        (n1 (big:pos (vslice random-values n2 (+ n2 n)))))
    (big-div n0 n1)))

;; I ran this with various values for the cutoff, rather than mutating the global cutoff
;; value from here - in case making it a mutable global (rather than a constant) affected
;; the results.

;; this tests values between 2 to 500 digits, writes CSV to stdout.

(for-range* i 2 50
  ;;(printf "\"" (int (* i 10)) "\",\"" (int (timeit (div-size (* 10 i)))) "\"\n")
  (printf "\"" (int (timeit (div-size (* 10 i)))) "\"\n")
  )
