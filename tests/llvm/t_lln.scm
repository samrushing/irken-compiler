;; -*- Mode: Irken -*-

(define (printn x)
  (%%cexp ('a -> undefined) "dump_object (%0, 0); fprintf (stdout, \"\\n\")" x))

(define (+ a b)
  (%llarith add a b))

(define ^llvm-x
  (:a n) -> (+ 1 n)
  (:b n) -> 34
  (:c n) -> (+ n 2)
  )

(printn (^llvm-x (:a 3)))
(printn (^llvm-x (:b 3)))
(printn (^llvm-x (:c 3)))






