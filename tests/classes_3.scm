
(define (printn x)
  (%%cexp (? -> undefined) "dump_object (%s, 0); fprintf (stdout, \"\\n\")" x))

(define (+ x y)
  (%%cexp (int int -> int) "%s+%s" x y))

(class thing (ob size:int))

(let ((o1 (thing "hello" 5))
      (o2 (thing #f 1)))
  (printn o1.ob)
  ;; this should fail - o1.ob is not a bool
  (if o2.ob
      (printn "yes")
      (printn "no"))
  (printn o1.ob)
  (printn (+ o1.size o2.size)))
