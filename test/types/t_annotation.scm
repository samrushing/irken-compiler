;; -*- Mode: Irken -*-

;; type annotations that agree with the code are accepted: on functions
;;  (inlined or not, polymorphic or not) and on let bindings.

(include "lib/basis.scm")

(define (ident x) : ('a -> 'a) x)
(define (len2 l) : ((list 'a) -> int) (length l))
(define (add1 x) : (int -> int) (+ x 1))
(printn (list (ident 1) (string-length (ident "ab")) (len2 (list 1 2)) (len2 (list "a"))))
(printn (map add1 (list 1 2 3)))
(let ((x:int 3)
      (l:(list string) (list "a" "b")))
  (printn (+ x (length l))))
;; => (1 2 2 1)
;; => (2 3 4)
;; => 5
