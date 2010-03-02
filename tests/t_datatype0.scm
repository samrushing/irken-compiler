
(datatype list
  (:nil)
  (:cons 'a (list 'a))
  )

(define (+ a b)
  (%%cexp (int int -> int) "%s+%s" a b))

(define (printn x)
  (%%cexp ('a -> undefined) "dump_object (%s, 0); fprintf (stdout, \"\\n\")" x))

(define (length l)
  (let loop ((l l) (r 0))
    (vcase list l
       ((:nil) r)
       ((:cons _ tl) (loop tl (+ r 1))))))

(let ((l0 (list:cons 1 (list:cons 2 (list:cons 3 (list:cons 4 (list:nil))))))
      (l1 (list:nil))
      )
  (printn l0)
  (printn (length l0))
  (set! l1 (list:cons #t l1))
  (printn l1)
  )


