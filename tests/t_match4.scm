
(datatype list
  (:nil)
  (:cons 'a (list 'a)))

(define (error)
  (%%cexp (-> 'a) "goto Lreturn")
  (%%cexp (-> 'a) "PXLL_UNDEFINED")
  )

(define thing
  (list:cons x xs) -> x
  (list:nil) -> (error)
  )

(thing '(1 2 3))
