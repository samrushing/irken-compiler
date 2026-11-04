;; -*- Mode: Irken -*-

(datatype list
  (:nil)
  (:cons 'a (list 'a))
  )

(define (+ a b) (%llarith add a b))
(define (* a b) (%llarith mul a b))
(define (= a b) (%llicmp eq a b))
(define (eq? a b) (%llicmp eq a b))

(define length*
  ()        a -> a
  (hd . tl) a -> (length* tl (+ 1 a))
  )

(define (conso x l)
  (list:cons  x l))

(length* (conso 1 (conso 2 (conso 3 (list:nil)))) 0)

