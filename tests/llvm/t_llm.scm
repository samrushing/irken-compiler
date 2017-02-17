;; -*- Mode: Irken -*-

(define (printn x)
  (%%cexp ('a -> undefined) "dump_object (%0, 0); fprintf (stdout, \"\\n\")" x))

(datatype thing
  (:a int)
  (:b int)
  (:c int)
  )

(define (+ a b)
  (%llarith add a b))

(define ^llvm-x
  (thing:a n) -> (+ 1 n)
  (thing:b n) -> 34
  (thing:c n) -> (+ n 2)
  )

(printn (^llvm-x (thing:a 3)))
(printn (^llvm-x (thing:b 3)))
(printn (^llvm-x (thing:c 3)))






