
(datatype list
  (:nil)
  (:cons 'a (list 'a))
  )

(datatype symbol (:t string))

(define (eq? a b)
  (%%cexp ('a 'a -> bool) "%s==%s" a b))

(define parse
  ()          -> 0
  ('expr . _) -> 1
  _           -> 2
  )

(define (printn x)
  (%%cexp ('a -> undefined) "dump_object (%s, 0); fprintf (stdout, \"\\n\")" x))

(let ((x '(expr a b c)))
  (printn x)
  (parse x))
