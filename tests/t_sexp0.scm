;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")

(pp (sexp (int 1) (sym 'x) (char #\a) (sexp (int 0) (int 3) (string "test"))) 20)
(pp (sexp (rec (a (sym 'int)) (b (sym 'list)))) 20)
(define list0 (sexp (int 0) (int 1) (int 2)))
(define list1 (sexpl (int 0) (int 1) (int 2)))
(pp (sexp (string "[") list0 (string "]")) 20)
(pp (sexp (string "[") (list list1) (string "]")
          (undef) (cons 'list 'nil)
          (attr (sym 'x) 'name)
          (attr (sexp (int 0) (int 1)) 'name)
          (vec (int 0) (int 1) (int 2))
          )
    10)
