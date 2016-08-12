;; -*- Mode: Irken -*-

;; goal: if a datatype has only one arm, the compiler should *not*
;;   emit an nvcase for it. (NYI 2016.08)

(define (printn x)
  (%%cexp ('a -> undefined) "dump_object (%0, 0); fprintf (stdout, \"\\n\")" x))

(define (+ a b)
  (%%cexp (int int -> int) "%0+%1" a b))

(datatype thing
  (:t {a=int b=int})
  )

(define add-thing
  (thing:t {a=a b=b})
  -> (+ a b))

(printn (add-thing (thing:t {a=1 b=3})))
