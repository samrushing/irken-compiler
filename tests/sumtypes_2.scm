
(define (printn x)
  (%%cexp (? -> undefined) "dump_object (%s, 0); fprintf (stdout, \"\\n\")" x))

(define (+ x y)
  (%%cexp (int int -> int) "%s+%s" x y))

(define (* x y)
  (%%cexp (int int -> int) "%s*%s" x y))

(datatype list
  (union 
    (empty)
    (pair ? list)
    ))

(define (length l)
  (let loop ((l l)
	     (a 0))
    (typecase list l
       ((empty) a)
       ((pair hd tl) (loop tl (+ a 1))))))

(let* ((x (list/pair 8 (list/pair 19 (list/empty))))
       (y (list/pair #t (list/pair #f (list/empty))))
       )
  (printn x)
  (printn (length x))
  )

