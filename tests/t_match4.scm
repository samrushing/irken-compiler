
(datatype list
  (:nil)
  (:cons 'a (list 'a)))

(define (error)
  (%exit #f #u)
  )

(define thing
  (list:nil) -> (error)
  (list:cons x _) -> x
  )

(thing '(1 2 3))


