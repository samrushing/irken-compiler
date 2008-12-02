
(define (zero? x)
  (%%cexp (int -> bool) "%s==0" x))

(define (identity x:?) x)

(if (identity (zero? 0))
    (identity 11)
    (identity 22))
