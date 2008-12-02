(define (cons a b)
  ;; rather than using a let, I'm trying to keep this small enough to inline
  ;; maybe later when I figure out hoisting, the more obvious defn will do.
  (%%cexp "( t = %s, ((pxll_pair*)t)->car = %s, ((pxll_pair*)t)->cdr = %s, t)" (%make-tuple #x18 2) a b))

(define (car x)
  (%%verify "TC_PAIR" 1 x)
  (%%cexp "((pxll_pair*)%s)->car" x))
(define (cdr x)
  (%%verify "TC_PAIR" 1 x)
  (%%cexp "((pxll_pair*)%s)->cdr" x))

;; ==================================================
;; list/pair
;; ==================================================	 

(define (car x) (car x))
(define (cdr x) (cdr x))
(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))
(define (set-car! l v)
  (%%cexp "(pxll_pair*)%s->car = %s" l v))
(define (set-cdr! l v)
  (%%cexp "(pxll_pair*)%s->cdr = %s" l v))
(define (null? x) (%eq? x '()))
		 
(define (pair? x)
  (%eq? (%get-typecode x) #x18))

;; http://groups.google.com/group/comp.lang.scheme/msg/0055f311d1e1ce08
(define (reverse-onto list1 list2)
  ;; reverses list1 onto the front of list2  
  (if (null? list1)
      list2
      (reverse-onto (cdr list1)
		    (cons (car list1) list2))))

(define (reverse l)
  (reverse-onto l '()))

(define (append list1 list2)
  (reverse-onto (reverse list1) list2))

(%printn (cons 1 2))
(%printn (cons 3141 '()))
(cons 1 2)
(%printn (car (cons 1 2)))
(%printn (cdr (cons 1 2)))
(%printn (cons 1 (cons 2 (cons 3 '()))))
(%printn (reverse               (cons 1 (cons 2 (cons 3 '())))))
(%printn (reverse-onto          (cons 1 (cons 2 (cons 3 '())))  (cons 4 (cons 5 (cons 6 '())))))
(%printn (reverse-onto (reverse (cons 1 (cons 2 (cons 3 '())))) (cons 4 (cons 5 (cons 6 '())))))
(%printn (append                (cons 1 (cons 2 (cons 3 '())))  (cons 4 (cons 5 (cons 6 '())))))
