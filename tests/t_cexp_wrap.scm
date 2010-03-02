(define (printn x)
  (%%cexp ('a -> undefined) "dump_object (%s, 0); fprintf (stdout, \"\\n\")" x))

(define (string-length s)
  (%%cexp ((raw string) -> int) "%s->len" s))

(define (thing)
  (%%cexp (-> int) "42"))

(printn (thing))
(printn (string-length "howdy"))
