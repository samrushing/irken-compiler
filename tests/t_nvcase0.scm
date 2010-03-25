
(datatype list
  (:nil)
  (:cons 'a (list 'a))
  (:blurt 'a)
  )

(define (printn x)
  (%%cexp ('a -> undefined) "dump_object (%s, 0); fprintf (stdout, \"\\n\")" x))

(define (error x)
  (printn x)
  (%%cexp (-> 'a) "goto Lreturn")
  (%%cexp (-> 'a) "PXLL_UNDEFINED")
  )

(define (thing x)
  (vcase list x
    ((:cons hd tl) hd)
    (else (error "empty list?"))))

(thing '(1 2 3))
