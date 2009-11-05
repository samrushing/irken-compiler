;; testing variants

(define (+ a b)
  (%%cexp (int int -> int) "%s+%s" a b))

(let ((x (%vcon/fnord 12))
      (y (%vcon/blort #f))
      )
  (vcase x
    ((fnord a) (+ a 5))
    ((blort b) (if b 9 8))
    ))
  