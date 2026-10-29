;; -*- Mode: Irken -*-

(include "lib/basis.scm")

(printn (list (< 1 2) (< 2 1) (<= 2 2) (> 3 2) (>= 2 3) (= 4 4) (= 4 5)))
;; => (#t #f #t #t #f #t #f)

;; comparisons on negative numbers (the tag bit must not get in the way)
(printn (list (< -5 -4) (< -1 0) (> 0 -1) (= -7 -7)))
;; => (#t #t #t #t)

(printn (list (eq? 'a 'a) (eq? 'a 'b) (eq? #t #t) (eq? #\x #\x)))
;; => (#t #f #t #t)

(printn (list (not #t) (not #f) (and #t #f) (or #f #t)))
;; => (#f #t #f #t)

;; int-cmp and magic-cmp return a cmp datatype
(define cmp->sym
  (cmp:<) -> 'lt
  (cmp:=) -> 'eq
  (cmp:>) -> 'gt)
(printn (map cmp->sym (list (int-cmp 1 2) (int-cmp 2 2) (int-cmp 3 2))))
(printn (map cmp->sym (list (magic-cmp "abc" "abd") (magic-cmp "x" "x") (magic-cmp (list 2) (list 1)))))
;; => (lt eq gt)
;; => (lt eq gt)

;; cond, with else
(define (sign n)
  (cond ((< n 0) 'negative)
        ((= n 0) 'zero)
        (else 'positive)))
(printn (map sign (list -3 0 3)))
;; => (negative zero positive)
