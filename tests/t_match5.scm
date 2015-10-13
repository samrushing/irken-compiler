
(define (eq? a b)
  (%%cexp ('a 'a -> bool) "%0==%1" a b))

(define (error x)
  (%exit #f x)
  )

(define flip
  0 -> 1
  1 -> 0
  x -> (error "flipped out!")
  )

(flip 0)
