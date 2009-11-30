;; testing variants
;; this will currently fail because case <else> is not yet implemented

(define (+ a b)
  (%%cexp (int int -> int) "%s+%s" a b))

(let ((x (:fnord 12))
      (y (:blort #f)))
  (vcase y
    ((:fnord a) (+ a 5))
    ((:blort b) (if b 9 8))
    (else 12)
    ))
