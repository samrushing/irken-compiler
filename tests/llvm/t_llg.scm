;; -*- Mode: Irken -*-

(datatype list
  (:cons 'a (list 'a))
  (:nil)
  )

(define (eq? a b)
  (%llicmp eq a b))

(define (- a b)
  (%llarith sub a b))

(define ^llvm-N
  0 acc -> acc
  n acc -> (^llvm-N (- n 1) (list:cons n acc))
  )

(^llvm-N 10 '())
