(define (+ a b)
  (%%cexp (int int -> int) "%s+%s" a b))

(define (thing x)
  x.add2)

(let ((ob0 {add2=(lambda (x) (+ x 2))})
      (th (thing ob0))
      (ob1 {add2=34})
      (xy (thing ob1)))
  (th xy))

