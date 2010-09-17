
(include "lib/core.scm")
(include "lib/pair.scm")

;; #t > #f
(define (bool->? a b)
  (if a #t #f))

(define (list/insert x > l)
  (match l with
    ()        -> (list:cons x l)
    (hd . tl) -> (if (> hd x)
		     (list:cons x l)
		     (list:cons hd (list/insert x > tl)))
    ))

(let ((l0 (LIST 20))
      (l1 (LIST #f))
      )
   (set! l0 (list/insert 1 > l0))
   (printn l0)
   (set! l0 (list/insert 5 > l0))
   (printn l0)
   (set! l1 (list/insert #f bool->? l1))
   (printn l1)
   (set! l1 (list/insert #t bool->? l1))
   (printn l1)
  )

