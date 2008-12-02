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
  (cexp "(pxll_pair*)%s->car = %s" l v))
(define (set-cdr! l v)
  (cexp "(pxll_pair*)%s->cdr = %s" l v))
		 
(define (pair? x)
  (%eq? (%get-typecode x) #x18))

;; incomplete, doesn't check for circular lists

(define (list? l)
  (or (null? l)
      (and (pair? l) (list? (cdr l)))))

(define (tuple->list t)
  (let ((len (%tuple-length t)))
    (let loop ((result '())
	       (index (%- len 1)))
      (if (%eq? index 0)
	  (%cons (%tuple-ref t index) result)
	  (loop (%cons (%tuple-ref t index) result)
		(%- index 1))))))

(define (list->tuple l tag)
  (let ((r (%new-tuple tag (length l))))
    (let loop ((i 0)
	       (l l))
      (if (%eq? l '())
	  r
	  (begin
	    (%tuple-set! r i (car l))
	    (loop (%+ i 1) (cdr l)))))))

(define (list)
  (cdr (tuple->list (%get-environment))))

(define (list-ref l i)
  (let loop ((l l) (i i))
    (if (%eq? i 0)
	(car l)
	(loop (cdr l) (%- i 1)))))

(define (length l)
  (let loop ((l l)
	     (n 0))
    (if (pair? l)
	(loop (cdr l) (%+ n 1))
	n)))

(define (append a b)
  (if (null? a)
      b
      (let loop ((l a))
	(if (null? (cdr l))
	    (%cons (car l) b)
	    (%cons (car l) (loop (cdr l)))))))

(define (reverse l)
  (let loop ((l l)
	     (r '()))
    (if (pair? l)
	(loop (cdr l) (%cons (car l) r))
	r)))

(define (member-pred pred)
  (lambda (x l)
    (let loop ((l l))
      (if (pair? l)
	  (if (pred x (car l))
	      l
	      (loop (cdr l)))
	  #f))))
			
(define member #f)
(define memv #f)

(define (memq x l) (%memq x l))
(define (assq x l) (%assq x l))

(define (assoc-pred pred)
  (lambda (x l)
    (let loop ((l l))
      (if (pair? l)
	  (if (pred x (car (car l)))
	      (car l)
	      (loop (cdr l)))
	  #f))))
