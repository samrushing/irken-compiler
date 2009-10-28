
(define (= a b)
  (%%cexp (int int -> bool) "%s==%s" a b))

(define (ident x) x)

(define (test)
  (if (ident (= 1 1))
      (ident 3)
      4
      ))

(test)
