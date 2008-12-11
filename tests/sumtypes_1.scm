
(define (printn x)
  (%%cexp (? -> undefined) "dump_object (%s, 0); fprintf (stdout, \"\\n\")" x))

(define (+ x y)
  (%%cexp (int int -> int) "%s+%s" x y))

(define (* x y)
  (%%cexp (int int -> int) "%s*%s" x y))

(datatype exp
  (union 
    (literal int)
    (sum exp exp)
    (prod exp exp)
    ))

(define (exp-value n)
  (typecase exp n
    ((literal v) v)
    ((sum a b) (+ (exp-value a) (exp-value b)))
    ((prod a b) (* (exp-value a) (exp-value b)))))

(let* ((x (exp/literal 19))
       (y (exp/sum x x))
       (z (exp/prod y y))
       )
  (printn x)
  (printn y)
  (printn z)
  (printn (exp-value z))
  )

