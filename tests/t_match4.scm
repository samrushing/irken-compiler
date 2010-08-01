
(datatype list
  (:nil)
  (:cons 'a (list 'a)))

(define (error)
  (%%cexp (-> 'a) "goto Lreturn")
  (%%cexp (-> 'a) "PXLL_UNDEFINED")
  )

(define thing
  (list:nil) -> (error)
  (list:cons x _) -> x
  )

(thing '(1 2 3))


