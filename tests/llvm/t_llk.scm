;; -*- Mode: Irken -*-

(datatype list
  (:nil)
  (:cons 'a (list 'a))
  )


(define (^llvm-x) '("thing1" "thing2"))

(^llvm-x)


