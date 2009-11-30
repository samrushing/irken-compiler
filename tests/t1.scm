(define (+ a b)
  (%%cexp (int int -> int) "%s+%s" a b))

(define (thing m)
  (+ 3 m.left))

(let ((x {right=#\A left=19}))
  (let ((y {right=(lambda (c) c)}))
    (let ((z y.right))
      (z (z (thing x))))))
