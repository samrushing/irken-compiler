;; -*- Mode: Irken -*-

;; bignum right shift, especially by more bits than the number has:
;;  (big-rshift 3 63) used to fail with a vector index out of range.

(include "lib/basis.scm")
(include "lib/bignum.scm")

(define A (dec->big "123456789012345678901234567890123456789"))

(printf (big->dec (big-rshift (int->big 3) 1)) "\n")
(printf (big->dec (big-rshift (int->big 3) 2)) "\n")
(printf (big->dec (big-rshift (int->big 3) 59)) "\n")
(printf (big->dec (big-rshift (int->big 3) 60)) "\n")
(printf (big->dec (big-rshift (int->big 3) 61)) "\n")
(printf (big->dec (big-rshift (int->big 3) 63)) "\n")
(printf (big->dec (big-rshift (int->big 3) 120)) "\n")
(printf (big->dec (big-rshift (int->big 3) 121)) "\n")
(printf (big->dec (big-rshift (big-lshift big/1 60) 59)) "\n")
(printf (big->dec (big-rshift (big-lshift big/1 60) 60)) "\n")
(printf (big->dec (big-rshift (big-lshift big/1 60) 61)) "\n")
(printf (big->dec (big-rshift (big-lshift big/1 60) 120)) "\n")
(printf (big->dec (big-rshift (big-lshift big/1 100) 99)) "\n")
(printf (big->dec (big-rshift (big-lshift big/1 100) 100)) "\n")
(printf (big->dec (big-rshift (big-lshift big/1 100) 101)) "\n")
(printf (big->dec (big-rshift (big-lshift big/1 100) 160)) "\n")
(printf (big->dec (big-rshift A 1)) "\n")
(printf (big->dec (big-rshift A 60)) "\n")
(printf (big->dec (big-rshift A 64)) "\n")
(printf (big->dec (big-rshift A 120)) "\n")
(printf (big->dec (big-rshift A 126)) "\n")
(printf (big->dec (big-rshift A 127)) "\n")
(printf (big->dec (big-rshift A 200)) "\n")
(printf (big->dec (big-rshift (big-lshift A 180) 180)) "\n")
(printf (big->dec (big-rshift big/0 77)) "\n")
;; => 1
;; => 0
;; => 0
;; => 0
;; => 0
;; => 0
;; => 0
;; => 0
;; => 2
;; => 1
;; => 0
;; => 0
;; => 2
;; => 1
;; => 0
;; => 0
;; => 61728394506172839450617283945061728394
;; => 107081695084215790682
;; => 6692605942763486917
;; => 92
;; => 1
;; => 0
;; => 0
;; => 123456789012345678901234567890123456789
;; => 0
