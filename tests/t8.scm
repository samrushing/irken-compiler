;; testing variant with else clause

(define (+ a b)
  (%%cexp (int int -> int) "%s+%s" a b))

(let ((w (:doubleu))
      (x (:fnord 12))
      (y (:blort #f))
      (z (:norg))
      )
  (vcase z
    ((:fnord a) (+ a 5))
    ((:blort b) (if b 9 8))
    ((:doubleu) 99)
    (else 12)
    ))
