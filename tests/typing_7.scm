;;
;; non-free typevar
;;
;; we *expect* a type error here...

(define (+ x y)
  (%%cexp (int int -> int) "%s+%s" x y))

(define (f x)
  (define (g y) x)
  (if (g 3)
      (g #t)
      (+ x 5)))

(f 2)

