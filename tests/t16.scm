;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")
(include "lib/random.scm")
(include "lib/frb.scm")

(define n-random
  t 0 -> t
  t n -> (n-random (tree/insert t < (random) (random)) (- n 1)))

(define indent
  0 -> #u
  n -> (begin (print-string "  ") (indent (- n 1))))

(define (print-item k v d)
  (indent d)
  (print k)
  (print-string ":")
  (print v)
  (print-string "\n"))

(define tree/print
  d (tree:empty)	  -> #u
  d (tree:red l r k v)    -> (begin (tree/print (+ d 1) l) (print-item k v d) (tree/print (+ d 1) r))
  d (tree:purple l r k v) -> (begin (tree/print (+ d 1) l) (print-item k v d) (tree/print (+ d 1) r))
  )

(define (print-kv k v)
  (print k)
  (print-string " ")
  (print v)
  (print-string "\n"))

(srandom 314159)

(let ((t (n-random (tree:empty) 20))
      (t2 (tree:empty))
      )
  (print-string "inorder:\n")
  (tree/inorder print-kv t)
  (print-string "reverse:\n")
  (tree/reverse print-kv t)
  (set! t (tree/insert t < 1234 5000))
  (printn (tree/member t < 1234))
  (printn (tree/member t < 9999))
  (printn t)
  (tree/print 0 t)
  (set! t2 (tree/insert t2 string<? "howdy" 0))
  (set! t2 (tree/insert t2 string<? "there" 2))
  (tree/print 0 t2)
  (let ((probe (tree/member t2 string<? "there")))
    (vcase maybe probe
      ((:no) (printn "nope") #t)
      ((:yes val) (printn val) #t)))
  )
