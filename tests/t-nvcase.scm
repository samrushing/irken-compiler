;; -*- Mode: Irken -*-

(datatype list
  (:nil)
  (:cons 'a (list 'a))
  )

(define (+ a b) (%llarith add a b))

(define length
  () a        -> a
  (hd . tl) a -> (length tl (+ 1 a))
  )

(define (num n)
  (+ n 1))

;; avoiding litcon here.
(length
 (list:cons
  (num 1)
  (list:cons
   (num 2)
   (list:cons
    (num 3)
    (list:nil))))
 0)




