
(define (+ x y)
  (%%cexp (int int -> int) "%s+%s" x y))

(define (cons a b)
  (%%make-tuple pair userobj a b))

(define (car a)
  (%%cexp (pair -> ?) "UOBJ_GET (%s, 0)" a))

(define (cdr a)
  (%%cexp (pair -> pair) "UOBJ_GET (%s, 1)" a))

;(define empty-list (%%cexp (-> pair) "PXLL_NIL"))

(car (cdr (cons 9 (cons 3 '()))))
