;; -*- Mode: Irken -*-

(include "lib/core.scm")

 ;; #x1001 0001 0000 0000 0001
(printn (bit-get #x1001 0))
(printn (bit-get #x1001 1))
(printn (bit-get #x1001 2))
(printn (bit-get #x1001 3))
(printn (bit-get #x1001 4))
(printn (bit-get #x1001 5))
(printn (bit-get #x1001 6))
(printn (bit-get #x1001 7))
(printn (bit-get #x1001 8))
(printn (bit-get #x1001 9))
(printn (bit-get #x1001 10))
(printn (bit-get #x1001 11))
(printn (bit-get #x1001 12))
(printn (bit-get #x1001 13))
(printn (bit-get #x1001 14))
(printn (bit-get #x1001 15))
(printn (bit-get #x1001 16))

(printn (bit-set 0 12))
(printn (bit-set 3141 7))
