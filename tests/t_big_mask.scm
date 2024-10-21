;; -*- Mode: Irken -*-

(require "lib/basis.scm")
(require "lib/bignum.scm")
(require "lib/codecs/hex.scm")

(define (hex->big s)
  (u256->big (hex->string s)))

(define (big->hex n)
  (string->hex (big->u256 n)))

(let ((n (hex->big "deadbeefcafebabe0123456789abcdef")))
  (printf (big->hex n) "\n")
  (for-range* i 17 130
    (printf (big->hex (big-mask-bits n i)) "\n")
    ))

(printf (big->hex (big-mask-bits big/1 256)) "\n")

;; # -*- Mode: Python -*-
;;
;; n = 0xdeadbeefcafebabe0123456789abcdef
;;
;; def phex (n):
;;     s = '%x' % (n,)
;;     if len(s) & 1:
;;         s = '0' + s
;;     return s
;;
;; print (phex(n))
;; for i in range (17, 130):
;;     print (phex(n & ((1 << i) - 1)))
