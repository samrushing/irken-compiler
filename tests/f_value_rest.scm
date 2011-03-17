;; value restriction

;(datatype bool (:false) (:true))

(define (+ a b)
  (%%cexp (int int -> int) "%0+%1" a b))

(let ((id (lambda (x) x)))
  (set! id (lambda (x) (+ 1 x)))
  ;; shouldn't be able to add one to #t
  (id #t)
  )
