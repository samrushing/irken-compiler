;; -*- Mode: Irken -*-

;; what the value restriction leaves polymorphic: functions and
;;  constructors applied to values.  [also NULL, a constant pointer: the
;;  zlib code uses it at two pointer types.]

(include "lib/basis.scm")

(define (ident x) x)
(define empty (list:nil))
(define none (maybe:no))

(printn (list (ident 1) (string-length (ident "abc"))))
(printn (list (length (list:cons 1 empty)) (length (list:cons "a" empty))))
(printn (list (maybe:yes 1) none))
;; => (1 3)
;; => (1 1)
;; => ({u0 1} <u1>)
