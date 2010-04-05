
(define (+ a b)
  (%%cexp (int int -> int) "%s+%s" a b))

(define (thing1 x)
  (define (thing2)
    (+ 10 x)
    )
  (thing2)
  )

(thing1 5)




