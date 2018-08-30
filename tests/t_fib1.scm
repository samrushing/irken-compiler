;; -*- Mode: Irken -*-

;; compute a term of F(n) directly.

;; based on:
;; https://rosettacode.org/wiki/Fibonacci_sequence#O.28log.28n.29.29_with_arbitrary_precision

(include "lib/basis.scm")
(include "lib/map.scm")
(include "demo/bignum.scm")

(define fib-mul
  (:tuple a b c) (:tuple d e f)
  -> (let ((bxe (big (* b e))))
       (:tuple (big (+ (* a d) bxe))
               (big (+ (* a e) (* b f)))
               (big (+ bxe (* c f)))))
  )

(define ID (:tuple big/1 big/0 big/1))

(define (fib-pow a n)
  (if (= 0 n)
      ID
      (let ((b (fib-pow a (/ n 2))))
        (if (even? n)
            (fib-mul b b)
            (fib-mul a (fib-mul b b))))))

(define (fib n)
  (match (fib-pow (:tuple big/1 big/1 big/0) n) with
    (:tuple _ y _) -> y
    ))

(printf "(fib 300) => " (big->dec (fib 300)) "\n")

;; --------------------------------------------------------------------------------
;; probably the same algorithm?
;; https://rosettacode.org/wiki/Fibonacci_sequence#Dijkstra_Algorithm

(define (dijkstra n)
  (define (fib-aux a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (fib-aux a
                    b
                    (big (+ (* p p) (* q q)))
                    (big (+ (* q q) (* big/2 p q)))
                    (/ count 2)))
          (else
           (fib-aux (big (+ (* b q) (* a q) (* a p)))
                    (big (+ (* b p) (* a q)))
                    p
                    q
                    (- count 1)))))
  (fib-aux big/1 big/0 big/0 big/1 n))

(printf "(dij 300) => " (big->dec (dijkstra 300)) "\n")

;; --------------------------------------------------------------------------------
;; close, but with large N `fib` wins.
;;
;; fib: 88688
;; dij: 103376
;;
;; (define (ticks)
;;   (%%cexp (-> int) "(pxll_int)rdtsc()"))
;;
;; (defmacro timeit
;;   (timeit body ...)
;;   -> (let (($t0 (ticks)))
;;        body ...
;;        (- (ticks) $t0)))
;;
;; (printf "fib: " (int (timeit (fib 1000))) "\n")
;; (printf "dij: " (int (timeit (dijkstra 1000))) "\n")
