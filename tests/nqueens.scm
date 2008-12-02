;;; NQUEENS -- Compute number of solutions to 8-queens problem.

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

(define (null? x)
  (%eq? x '()))

(define (append a b)
  (if (null? a)
      b
      (let loop ((l a))
	(if (null? (cdr l))
	    (cons (car l) b)
	    (cons (car l) (loop (cdr l)))))))

;; http://groups.google.com/group/comp.lang.scheme/msg/0055f311d1e1ce08
(define (reverse-onto list1 list2)
  ;; reverses list1 onto the front of list2  
  (if (null? list1)
      list2
      (reverse-onto (cdr list1)
		    (cons (car list1) list2))))

(define (reverse l)
  (reverse-onto l '()))

(define (xappend list1 list2)
  (reverse-onto (reverse list1) list2))

(define (not x)
  (%eq? x #f))

(define (+ x y)
  (%%verify "TC_INT" 1 x)
  (%%verify "TC_INT" 1 y)
  (%+ x y))

(define (- x y)
  (%%verify "TC_INT" 1 x)
  (%%verify "TC_INT" 1 y)
  (%- x y))

(define (nqueens n)

  (define (one-to n)
    (let loop ((i n) (l '()))
      (if (%eq? i 0) l (loop (- i 1) (cons i l)))))

  (define (try x y z)
    (if (null? x)
      (if (null? y) 1 0)
      (+ (if (ok? (car x) 1 z)
           (try (append (cdr x) y) '() (cons (car x) z))
           0)
         (try (cdr x) (cons (car x) y) z))))

  (define (ok? row dist placed)
    (if (null? placed)
      #t
      (and (not (%eq? (car placed) (+ row dist)))
           (not (%eq? (car placed) (- row dist)))
           (ok? row (+ dist 1) (cdr placed)))))

  (try (one-to n) '() '()))

(%printn (nqueens 12))
