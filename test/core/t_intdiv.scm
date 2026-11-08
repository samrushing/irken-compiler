;; -*- Mode: Irken -*-

;; integer division: the three families (truncate, floor, euclidean),
;;  each as X/, X-quotient and X-remainder, for every combination of
;;  signs, and the names that alias them.  each line is
;;  (n d  q r from X/  q from X-quotient  r from X-remainder), so the
;;  pairs can be seen to agree.  n = d*q + r is checked too (BAD if not).

(include "lib/basis.scm")

(define cases
  (list
   (:tuple 8 3) (:tuple 8 -3) (:tuple -8 3) (:tuple -8 -3) (:tuple 1 2) (:tuple 1 -2) (:tuple -1 2) (:tuple -1 -2) (:tuple 9 3) (:tuple -9 3) (:tuple 9 -3) (:tuple -9 -3) (:tuple 0 5) (:tuple 0 -5)))

(define (show name div quo rem)
  (printn name)
  (for-list c cases
    (let (((n d) c)
          ((q r) (div n d)))
      (printn (list n d q r (quo n d) (rem n d)))
      (when (not (= n (+ (* d q) r)))
        (printn 'BAD)))))

(show 'truncate truncate/ truncate-quotient truncate-remainder)
(show 'floor floor/ floor-quotient floor-remainder)
(show 'euclidean euclidean/ euclidean-quotient euclidean-remainder)

;; the other names: / mod divmod are euclidean; quotient and remainder
;;  truncate, modulo floors; and the older names still work.
(printn (list (/ -8 3) (mod -8 3) (/ 8 -3) (mod 8 -3)))
(printn (let (((q r) (divmod -8 -3))) (list q r)))
(printn (list (quotient -8 3) (remainder -8 3) (modulo -8 3) (modulo 8 -3)))
(printn (list (div-floored -8 3) (mod-floored 8 -3) (div-euclidean -8 -3) (mod-euclidean -8 -3)))
(printn (let (((q r) (divmod-truncated -8 3))) (list q r)))

;; dividing by zero raises :DivideByZero, from every one of them.
(define (zero-check f)
  (try
   (begin (f 1 0) 'no)
   except
   (:DivideByZero _) -> 'yes
   ))
(printn (map zero-check (list truncate-quotient truncate-remainder floor-quotient floor-remainder
                              euclidean-quotient euclidean-remainder / mod quotient remainder modulo)))
(printn (map zero-check (list (lambda (n d) (let (((q r) (truncate/ n d))) q))
                              (lambda (n d) (let (((q r) (floor/ n d))) q))
                              (lambda (n d) (let (((q r) (euclidean/ n d))) q)))))

;; => truncate
;; => (8 3 2 2 2 2)
;; => (8 -3 -2 2 -2 2)
;; => (-8 3 -2 -2 -2 -2)
;; => (-8 -3 2 -2 2 -2)
;; => (1 2 0 1 0 1)
;; => (1 -2 0 1 0 1)
;; => (-1 2 0 -1 0 -1)
;; => (-1 -2 0 -1 0 -1)
;; => (9 3 3 0 3 0)
;; => (-9 3 -3 0 -3 0)
;; => (9 -3 -3 0 -3 0)
;; => (-9 -3 3 0 3 0)
;; => (0 5 0 0 0 0)
;; => (0 -5 0 0 0 0)
;; => floor
;; => (8 3 2 2 2 2)
;; => (8 -3 -3 -1 -3 -1)
;; => (-8 3 -3 1 -3 1)
;; => (-8 -3 2 -2 2 -2)
;; => (1 2 0 1 0 1)
;; => (1 -2 -1 -1 -1 -1)
;; => (-1 2 -1 1 -1 1)
;; => (-1 -2 0 -1 0 -1)
;; => (9 3 3 0 3 0)
;; => (-9 3 -3 0 -3 0)
;; => (9 -3 -3 0 -3 0)
;; => (-9 -3 3 0 3 0)
;; => (0 5 0 0 0 0)
;; => (0 -5 0 0 0 0)
;; => euclidean
;; => (8 3 2 2 2 2)
;; => (8 -3 -2 2 -2 2)
;; => (-8 3 -3 1 -3 1)
;; => (-8 -3 3 1 3 1)
;; => (1 2 0 1 0 1)
;; => (1 -2 0 1 0 1)
;; => (-1 2 -1 1 -1 1)
;; => (-1 -2 1 1 1 1)
;; => (9 3 3 0 3 0)
;; => (-9 3 -3 0 -3 0)
;; => (9 -3 -3 0 -3 0)
;; => (-9 -3 3 0 3 0)
;; => (0 5 0 0 0 0)
;; => (0 -5 0 0 0 0)
;; => (-3 1 -2 2)
;; => (3 1)
;; => (-2 -2 1 -1)
;; => (-3 -1 3 1)
;; => (-2 -2)
;; => (yes yes yes yes yes yes yes yes yes yes yes)
;; => (yes yes yes)
