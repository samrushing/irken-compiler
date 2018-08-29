
(define (eq? a b)
  (%%cexp ('a 'a -> bool) "%0==%1" a b))

(define (error x)
  (%%cexp (-> 'a) "goto Lreturn")
  (%%cexp (-> 'a) "IRK_UNDEFINED")
  )

;; without a default case this should raise a match error

(define flip
  0 -> 1
  1 -> 0
;;  x -> (error "flipped out!")
  )

(flip 0)
