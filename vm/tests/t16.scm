;; -*- Mode: Irken -*-

(datatype list
  (:nil)
  (:cons 'a (list 'a))
  )

(list:cons 1 (list:cons 2 (list:nil)))
