;; -*- Mode: Irken -*-

;; tail calls must not grow the stack (or the continuation chain): these
;;  loops run far longer than any stack could hold.

(include "lib/basis.scm")

(define (count-up i n acc)
  (if (= i n)
      acc
      (count-up (+ i 1) n (+ acc 1))))
(printn (count-up 0 10000000 0))
;; => 10000000

(define (ev? n) (if (= n 0) #t (od? (- n 1))))
(define (od? n) (if (= n 0) #f (ev? (- n 1))))
(printn (list (ev? 1000000) (od? 1000001)))
;; => (#t #t)

;; a tail call through a closure
(define (trampoline f n)
  (if (= n 0)
      'done
      ((lambda (m) (trampoline f m)) (- n 1))))
(printn (trampoline id 1000000))
;; => done

;; a tail call in each arm of a match
(define loop3
  0 acc -> acc
  n acc -> (if (even? n)
               (loop3 (- n 1) (+ acc 2))
               (loop3 (- n 1) (+ acc 1))))
(printn (loop3 3000000 0))
;; => 4500000
