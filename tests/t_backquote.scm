;; -*- Mode: Irken -*-

(datatype list
  (:nil)
  (:cons 'a (list 'a)))

(define x 9)

;;`(1 2 3 ,x)

`((1 2 3)
  (4 5 6)
  (7 8 ,x)
  (10 11 12))
  