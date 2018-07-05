;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")
(include "lib/codecs/hex.scm")
(include "lib/crypto/sha1.scm")

(define (t0)
  (let ((strings '("testing" ", testing!\n")))
    (assert
     (string=?
      "fbf8c65de539027c21c84cd3544aaf18e8786c90"
      (string->hex (sha1 (list-generator strings)))))))

(define (t1)
  (define (gen)
    (makegen emit
      (for-range i 100
        (let ((ch (int->char i))
              (part (format (repeat 17 (char ch)))))
          (emit part)))))
  (assert
   (string=?
    "9cbe914e14e2ebeeb126ffda120638997e664774"
    (string->hex (sha1 (gen))))))

(t0)
(t1)

