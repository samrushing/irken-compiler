;; -*- Mode: Irken -*-

(datatype list
  (:nil)
  (:cons 'a (list 'a))
  )

(define (xfront l)
  (list:cons 'x l))

(xfront '(a b c))
