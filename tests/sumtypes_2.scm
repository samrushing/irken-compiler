
(define (printn x)
  (%%cexp (? -> undefined) "dump_object (%s, 0); fprintf (stdout, \"\\n\")" x))

(define (+ x y)
  (%%cexp (int int -> int) "%s+%s" x y))

(define (* x y)
  (%%cexp (int int -> int) "%s*%s" x y))

(datatype list
  (union 
    (empty int)
    (pair ? list)
    ))

(define (length l)
  (let loop ((l l)
	     (a 0))
    (typecase list l
       ((empty x) a)
       ((pair hd tl) (loop tl (+ a 1))))))

(let* ((x (list/pair 8 (list/pair 19 (list/empty 0))))
       (y (list/pair #t (list/pair #f (list/empty 0))))
       )
  (printn x)
  (printn (length x))
  )

