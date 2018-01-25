;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")

(printn (string-split-string "------aaaa------aaaa-------" "aaaa"))
(printn (string-split-string "------aaaa------aaaa-------" "bbbb"))
(printn (string-split-string "------aaaa------aaaa" "aaaa"))
(printn (string-split-string "------a------aaaa" "a"))
(printn (string-split-string "aaa---a---aaa---" "aaa"))
(printn (string-split-string "aaaaa---a---aaaaa---" "aaa"))

(printn (string-replace-all "aaaaa---a---aaaaa---" "aaa" "b"))

