
(define (+ a b)
  (%%cexp (int int -> int) "%s+%s" a b))

(define (length l)
  (let loop ((l l)
	     (n 0))
    (vcase l
       ((:nil) n)
       ((:cons _ tl)
	(loop tl (+ n 1))))))

(let ((l (:cons 1 (:cons 2 (:cons 3 (:nil))))))
  (length l))
