;; -*- Mode: Irken -*-

;; top-level definitions run in source order, except that whatever one
;;  depends on (here thing1, through g.foo -> foo) runs before it.
;;  thing2 used to call foo before thing1 was set, and crashed.  [#86]

(include "lib/basis.scm")

(define (foo)
  (match (alist/lookup thing1 2) with
    (maybe:yes ch) -> 99
    (maybe:no)     -> 77
    ))

(define (goober)
  {foo=foo}
  )

(define g (goober))

(define thing2
  (g.foo)
  )

(define thing1
  (alist/make
   (0 #\0)
   (1 #\1)
   (2 #\C)
   ))


;; definitions that don't depend on each other keep their order.
(define a (begin (printn 'a) 1))
(define b (begin (printn 'b) 2))
(define c (begin (printn 'c) (+ a b)))

;; (all the definitions run before the other top-level expressions.)
(printn thing2)
(printn c)
;; => a
;; => b
;; => c
;; => 99
;; => 3
