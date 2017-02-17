;; -*- Mode: Irken -*-

(datatype list
  (:cons 'a (list 'a))
  (:nil)
  )

(define (= a b)
  (%llicmp eq a b))

(define (- a b)
  (%llarith sub a b))

(define (^llvm-N n acc)
  (if (= n 0)
      acc
      (^llvm-N (- n 1) (list:cons n acc))))

(^llvm-N 10 '())
