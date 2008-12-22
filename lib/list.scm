
(datatype list
  (union
   (nil)
   (cons ? list)
   ))

(define (cons a b)
  (list/cons a b))

(define (null? l)
  (typecase list l
    ((nil) #t)
    ((cons _ _ ) #f)))

;; http://groups.google.com/group/comp.lang.scheme/msg/0055f311d1e1ce08

(define (reverse-onto l1 l2)
  (typecase list l1
     ((nil) l2)
     ((cons hd tl)
      (reverse-onto tl (cons hd l2)))
     ))

(define (reverse l)
  (reverse-onto l '()))

(define (append list1 list2)
  (reverse-onto (reverse list1) list2))

(define (length l)
  (let loop ((l l)
	     (n 0))
    (typecase list l
       ((nil) n)
       ((cons _ tl)
	(loop tl (+ n 1))))))

(define (memq x l)
  (typecase list l
     ((nil) #f)
     ((cons hd tl)
      (if (eq? hd x)
	  #t
	  (memq x tl)))))

;; hold off on assq until I get a 'product' datatype
