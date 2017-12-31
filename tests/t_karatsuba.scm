;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")
(include "demo/bignum.scm")
(include "lib/mtwist.scm")

;; verify the correctness of karatsuba multiplication.

;; random-bits generator never stops.
(define (always gen)
  (match (gen) with
    (maybe:yes val) -> val
    (maybe:no)      -> (impossible)
    ))

;; make `n` pairs of `m`-digit bignums.
(define (make-pairs n m)
  (let ((gen (generate-random-bits big/bits 3141)) ;; stream of big/base-sized ints
        (v0 (make-vector n (big:zero)))
        (v1 (make-vector n (big:zero))))
    (for-range i n
      (let ((n0 (big:pos (list->vector (map-range i m (always gen)))))
            (n1 (big:pos (list->vector (map-range i m (always gen))))))
        (set! v0[i] n0)
        (set! v1[i] n1)
        ))
    (:tuple v0 v1)))

;; multiply 1000 10-digit numbers.
(let (((av bv) (make-pairs 1000 10)))
  (for-range i 1000
    (printf "-----------------\n")
    (printf (big-repr av[i]) "\n")
    (printf (big-repr bv[i]) "\n")
    (let ((r0 (big:zero))
          (r1 (big:zero)))
      (set! KARATSUBA-CUTOFF 1000) ;; disable karatsuba
      (set! r0 (big-mul av[i] bv[i]))
      (set! KARATSUBA-CUTOFF 2)
      (set! r1 (big-mul av[i] bv[i]))
      (printf (big-repr r0) "\n")
      (assert (big= r0 r1))
      )))
