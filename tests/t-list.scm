;; -*- Mode: Irken -*-

(datatype list
  (:nil)
  (:cons 'a (list 'a))
  )

(define (conso x l)
  (list:cons  x l))

(conso 1 (conso 2 (conso 3 (list:nil))))
