;; -*- Mode: Irken -*-

(define (= a b)
  (%llicmp eq a b))

(define (+ a b)
  (%llarith add a b))

(define (- a b)
  (%llarith sub a b))

(define (^llvm-mv r)
  (+ r.x r.z)
  )

(define (printn x)
  (%%cexp ('a -> undefined) "dump_object (%0, 0); fprintf (stdout, \"\\n\")" x))

;; creating this similar record forces the use of lookup_field().
(define thing {x=3 y="fnord" z=8 a=12})

(printn thing)

(^llvm-mv {x=3 y="testing" z=7})

