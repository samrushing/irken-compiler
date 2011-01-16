(define (printn x)
  (%%cexp ('a -> undefined) "dump_object (%0, 0); fprintf (stdout, \"\\n\")" x))

(define (string-length s)
  (%%cexp ((raw string) -> int) "%0->len" s))

(define (thing)
  (%%cexp (-> int) "42"))

(printn (thing))
(printn (string-length "howdy"))
