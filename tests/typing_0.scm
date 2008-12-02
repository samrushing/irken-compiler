
;(define (> x y)
;  (%gt? x y))

(define (eq? x y)
  (%%cexp (? ? -> bool) "%s==%s" x y))

(define (+ x y)
  (%%cexp (int int -> int) "%s+%s" x y))

(define (make-tuple tag size)
  (%%cexp (int int -> ?) "allocate (%s, %s)" tag size))

(define (cons a b)
  (%%cexp (? ? ? -> pair) "( t = %s, ((pxll_pair*)t)->car = %s, ((pxll_pair*)t)->cdr = %s, t)" (make-tuple #x18 2) a b))

(define (car x)
  (%%cexp (pair -> ?) "((pxll_pair*)%s)->car" x))

(define (cdr x)
  (%%cexp (pair -> ?) "((pxll_pair*)%s)->cdr" x))

(define (thing l)
  (let ((x (car l)))
    (+ x x)))

(define (length l)
  (let loop ((l l)
	     (n 0))
    (if (eq? l '())
	n
	(loop (cdr l) (+ n 1)))))

(let ((x '(1 2 3)))
  (thing x)
  (length x))

;(+ (car (cons (+ 19 34) '())) 99)

;(define (> x y)
;  (%%cexp (int int -> bool) "%s > %s" x y))

;(define (thing x:int)
;  (let ((y (%* x 3)))
;    (set! y (%+ y 12))
;    (if (> x y)
;	(%+ 110 y)
;	(%- 110 y))))
;
;(thing (%+ 34 19))
