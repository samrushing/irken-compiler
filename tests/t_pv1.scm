;; -*- Mode: Irken -*-

(define (eq? a b)
  (%%cexp ('a 'a -> bool) "%0==%1" a b))

(define (+ a b)
  (%%cexp (int int -> int) "%0+%1" a b))

(define (- a b)
  (%%cexp (int int -> int) "%0-%1" a b))

(define klength
  (:nil) -> 0
  (:kons _ y) -> (+ 1 (klength y)))

(define kn
  0 -> (:nil)
  n -> (:kons n (kn (- n 1))))

(klength (kn 5))



