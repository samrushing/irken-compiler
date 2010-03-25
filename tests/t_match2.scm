
(datatype list
  (:nil)
  (:cons 'a (list 'a)))

(define map-pairs
  f (list:nil) ys                     -> (list:nil)
  f (list:cons x xs) (list:nil)       -> (list:nil)
  f (list:cons x xs) (list:cons y ys) -> (list:cons (f x y) (map-pairs f xs ys)))
  
;; original version:
;(define (reverse-onto l1 l2)
;  (vcase list l1
;    ((:nil) l2)
;    ((:cons hd tl)
;     (reverse-onto tl (list:cons hd l2)))
;    ))

;; pattern-matching version:
(define reverse-onto
  (list:nil) y       -> y
  (list:cons x xs) y -> (reverse-onto xs (list:cons x y))
  )

;; with list syntax help:
;(define reverse-onto
;  [] y        -> y
;  [x :: xs] y -> (reverse-onto xs [x :: y])
;  )

(define (reverse l)
  (reverse-onto l (list:nil)))

(define (+ a b)
  (%%cexp (int int -> int) "%s+%s" a b))

(reverse (map-pairs + '(1 2 3 4) '(5 6 7 8)))

