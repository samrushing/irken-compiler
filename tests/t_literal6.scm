
(datatype list
  (:nil)
  (:cons 'a (list 'a))
  )

(literal (list:cons "testing" (list:cons "one" (list:cons "two" (list:cons "three" (list:nil))))))


