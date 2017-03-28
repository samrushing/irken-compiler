;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")

(printn (list<? < '(88) '(99)))                ;; #t
(printn (list<? < '(99) '(88)))                ;; #f
(printn (list<? < '(99) '(99 21)))             ;; #t
(printn (list<? < '(99 21) '(99)))             ;; #f
(printn (list<? < '(1 2 3) '(1 2 3)))          ;; #f
(printn (list<? < '(1 2 3) '(0 2 3)))          ;; #f
(printn (list<? < '(0 2 3) '(1 2 3)))          ;; #t
(printn (list<? < '(1 2 3 4 5) '(1 2 3 4 6)))  ;; #t
(printn (list<? < '(0 2 3 4 6) '(1 2 3 4 5)))  ;; #t
(printn (list<? < '(1 2 3 4 5) '(1 2 4 9 8)))  ;; #t
(printn (list<? < '(1 2 4 9 8) '(1 2 3 4 5)))  ;; #f


