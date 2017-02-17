;; -*- Mode: Irken -*-

(datatype list
  (:nil)
  (:cons 'a (list 'a))
  )

;(define (+ a b)
;  (%%cexp (int int -> int) "%0+%1" a b))

(define (+ a b)
  (%llarith add a b))

(define ^llvm-len
  () acc       -> acc
  (_ . tl) acc -> (^llvm-len tl (+ 1 acc))
  )

(^llvm-len '(1 2 3 4 5) 0)




