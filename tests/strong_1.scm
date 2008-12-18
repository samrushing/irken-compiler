
(define (zero? n)
  (%%cexp (int -> bool) "%s==0" n))

(define (decrement x)
  (%%cexp (int -> int) "%s-1" x))

(define (even? x)
  (if (zero? x)
      #t
      (odd? (decrement x))))

(define (odd? x)
  (if (zero? x)
      #f
      (even? (decrement x))))

(even? 314)
