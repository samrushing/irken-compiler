;; -*- Mode: Irken -*-

;; trying to find a smaller repro of a bad strongly-generated ordering
;;   where the alist is undefined.

(require "lib/core.scm")
(require "lib/alist.scm")
(require "lib/pair.scm")

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

(printn thing1)
(printn thing2)


