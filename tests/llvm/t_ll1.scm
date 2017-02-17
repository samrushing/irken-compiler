;; -*- Mode: Irken -*-

(datatype bool
  (:true)
  (:false)
)

(define (printn x)
  (%%cexp ('a -> undefined) "dump_object (%0, 0); fprintf (stdout, \"\\n\")" x))

(define boolvar #t)

(define (^llvm-thing x)
  (if x
      19
      34))

(printn (^llvm-thing boolvar))
