;; -*- Mode: Irken -*-

;; int-cmp, which maps and sort use, must compare signed.

(include "lib/basis.scm")

(define cmp->sym
  (cmp:<) -> '<
  (cmp:=) -> '=
  (cmp:>) -> '>)

(printn (map (lambda (p) (cmp->sym (int-cmp (first p) (second p))))
             (list (list -1 1) (list 1 -1) (list -5 -5) (list -5 -4) (list -4 -5) (list 0 -1))))
;; => (< > = < > >)

;; negative keys in a map
(define t (tree/empty))
(for-list k (list 3 -1 0 -7 5 -2)
  (tree/insert! t int-cmp k k))
(printn (tree/keys t))
;; => (-7 -2 -1 0 3 5)

(printn (sort < (list 3 -1 0 -7 5 -2)))
;; => (-7 -2 -1 0 3 5)
