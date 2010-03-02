
(datatype list
  (nil)
  (cons 'a list)
  )

(let ((l0 (list:cons 4 (list:nil)))
      (l1 (list:cons #t (list:nil)))
      (l2 (list:cons "hey" (list:nil)))
      )
  (set! l0 (list:cons #t l0))
  l0
  )
