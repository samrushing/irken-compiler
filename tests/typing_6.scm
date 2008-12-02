
;; This will work when we get 'let-polymorphism' working.

(define (zero? x)
  (%%cexp (int -> bool) "%s==0" x))

(define (+ x y)
  (%%cexp (int int -> int) "%s+%s" x y))

(define (id x) x)

(if (id (zero? 0))
    (id (+ 11 3))
    (id 22))

