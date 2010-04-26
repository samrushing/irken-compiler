
(datatype list
  (:nil)
  (:cons 'a (list 'a))
  )

(define unwieldy
  () () -> (list:nil)
  x  y  -> (list:cons 10 (list:cons 9 (list:cons 8 x)))
  )

(unwieldy '(12) '(8 9 7))
;(unwieldy '() '())
