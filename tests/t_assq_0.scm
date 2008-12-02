(include "lib/pair.scm")

;; '((1 . 2) (3 . 4))

(let ((t (cons (cons 1 2) (cons (cons 3 4) '()))))
  (%printn t)
  (%printn (assq 1 t))
  (%printn (assq 2 t))
  (%printn (assq 3 t)))
