
(datatype list
  (:nil)
  (:cons 'a (list 'a))
  )

(define (eq? a b)
  (%%cexp ('a 'a -> bool) "%s==%s" a b))

(define parse
  (list:nil)          -> 0
  (list:cons 'expr x) -> 1
  (list:cons x y)     -> 2
  )

(parse '(expr a b c))
