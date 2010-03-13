(datatype list
  (:nil)
  (:cons 'a (list 'a)))

;; '((1 2) (3 4))

(literal
 (list:cons
  (list:cons 1 (list:cons 2 (list:nil)))
  (list:cons
   (list:cons 3 (list:cons 4 (list:nil)))
   (list:nil))))
