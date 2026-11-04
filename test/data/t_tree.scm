;; -*- Mode: Irken -*-

;; the red-black tree maps in lib/frb.scm

(include "lib/basis.scm")

(define t (tree/empty))
(for-list k (list 5 3 8 1 4 7 9 2 6)
  (tree/insert! t int-cmp k (* k 10)))

(printn (tree/keys t))
(printn (tree/values t))
(printn (tree/size t))
;; => (1 2 3 4 5 6 7 8 9)
;; => (10 20 30 40 50 60 70 80 90)
;; => 9

(define show-maybe
  (maybe:yes x) -> (format "yes " (int x))
  (maybe:no)    -> "no")
(printn (list (show-maybe (tree/member t int-cmp 4)) (show-maybe (tree/member t int-cmp 42))))
;; => ("yes 40" "no")

(tree/delete! t int-cmp 5)
(tree/delete! t int-cmp 1)
(printn (tree/keys t))
;; => (2 3 4 6 7 8 9)

;; inserting an existing key leaves the old value
(tree/insert! t int-cmp 2 999)
(printn (tree/get t int-cmp 2))
;; => 20

(for-map k v t
  (when (> k 7)
    (printf (int k) "=" (int v) "\n")))
;; => 8=80
;; => 9=90

;; string keys
(define s (tree/make string-compare ("one" 1) ("two" 2) ("three" 3)))
(printn (tree/keys s))
;; => ("one" "three" "two")
