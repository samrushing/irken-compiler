
(datatype list
  (:nil)
  (:cons 'a (list 'a))
  )

(let ((il:(list int) (list:nil))
      (bl:(list bool) (list:nil)))
  (set! il (list:cons 12 il))
  (set! bl (list:cons #f bl))
  il
  )
