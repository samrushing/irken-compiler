;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")
(include "lib/codecs/hex.scm")

(let ((s "603c303a040361626302013202022711020417bc927a3020300602010a020165300602010a020165300602010a020165300602010a0201650101000a0100"))
  (printn (hex->string s))
  (printn (string->hex (hex->string s)))
  (assert (string=? s (string->hex (hex->string s)))))
