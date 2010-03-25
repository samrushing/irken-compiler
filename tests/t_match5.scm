
(define (eq? a b)
  (%%cexp ('a 'a -> bool) "%s==%s" a b))

(define (error x)
  (%%cexp (-> 'a) "goto Lreturn")
  (%%cexp (-> 'a) "PXLL_UNDEFINED")
  )

(define flip
  0 -> 1
  1 -> 0
  x -> (error "flipped out!")
  )

(flip 0)
