;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")
(include "lib/format.scm")
(include "demo/sha256.scm")

(define (string->hex s)
  (format
   (join
    (map
     (lambda (x) (format (zpad 2 (hex x))))
     (map char->int (string->list s))))))

(assert
 (string=? "62812653f2165f5437bb2eae59819dbc4ecdbd40ece9cf62f6911d5b7de1074c"
           (string->hex (sha256 "onomatopoeia"))))
