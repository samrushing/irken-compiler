;; -*- Mode: Irken -*-

;; binding forms: let (parallel? no: sequential in irken), named let,
;;  letrec, inner defines, mutual recursion, set!.

(include "lib/basis.scm")

(let ((a 1)
      (b (+ 1 1))
      (c (+ a b)))       ;; irken's let binds sequentially, like let*
  (printn (list a b c)))
;; => (1 2 3)

(let loop ((i 0) (acc '()))
  (if (< i 5)
      (loop (+ i 1) (list:cons i acc))
      (printn acc)))
;; => (4 3 2 1 0)

(define (f x)
  (define (g y) (* y 10))
  (define (h z) (+ (g z) 1))
  (h x))
(printn (f 4))
;; => 41

(define (my-even? n) (if (= n 0) #t (my-odd? (- n 1))))
(define (my-odd? n) (if (= n 0) #f (my-even? (- n 1))))
(printn (list (my-even? 10) (my-odd? 7) (my-even? 7)))
;; => (#t #t #f)

(letrec ((ping (lambda (n) (if (= n 0) 'ping (pong (- n 1)))))
         (pong (lambda (n) (if (= n 0) 'pong (ping (- n 1))))))
  (printn (list (ping 4) (ping 5))))
;; => (ping pong)

(define counter 0)
(for-range i 10 (set! counter (+ counter i)))
(printn counter)
;; => 45

;; shadowing
(let ((x 1))
  (let ((x (+ x 1)))
    (printn x))
  (printn x))
;; => 2
;; => 1

;; a function defined by pattern matching, with a literal case
(define fib
  0 -> 0
  1 -> 1
  n -> (+ (fib (- n 1)) (fib (- n 2))))
(printn (fib 20))
;; => 6765
