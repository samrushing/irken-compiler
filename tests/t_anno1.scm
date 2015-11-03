
;; (define (thing:(int -> int) x)
;;   3)

(define (printn x)
  (%%cexp ('a -> undefined) "dump_object (%0, 0); fprintf (stdout, \"\\n\")" x))

(define (thing x) : (int -> int)
  (printn x)
  (printn x)
  (printn x)
  (printn x)
  (printn x)
  (printn x)
  (printn x)
  3)

(printn (thing 3))
(printn (thing 4))
(printn (thing 5))
