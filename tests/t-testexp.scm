;; -*- Mode: Irken -*-

(define (print x) (%asm-c-call ("irk_dump_object" ('a -> undefined)) x))
(define (newline) (%asm-c-call ("irk_putc" (char -> undefined)) #\newline))
(define (printn x) (print x) (newline))

(define (= a b) (%llicmp eq a b))
(define (> a b) (%llicmp sgt a b))
(define (- a b) (%llarith sub a b))

(define (^thing1 x)
  (if (= x 12)
      34
      19))

(define (^thing2 x y)
  (if (> x y)
      (- x y)
      (- y x)))

(printn (^thing1 1))
(printn (^thing1 2))
(printn (^thing1 3))
(printn (^thing1 12))

(printn (^thing2 12 7))
(printn (^thing2 7 3))
(printn (^thing2 2 100))
(printn (^thing2 50 50))
(printn (^thing2 49 51))

