;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")

(printn (string-split-string "------aaaa------aaaa-------" "aaaa"))
(printn (string-split-string "------aaaa------aaaa-------" "bbbb"))
(printn (string-split-string "------aaaa------aaaa" "aaaa"))
(printn (string-split-string "------a------aaaa" "a"))
(printn (string-split-string "aaa---a---aaa---" "aaa"))
(printn (string-split-string "aaaaa---a---aaaaa---" "aaa"))

(printn (string-split-string "header0: value0\r\nheader1: value1\r\nheader2: value2\r\n\r\n" "\r\n"))
(printn (string-split-string "header0: value0\r\nheader1: value1\r\nheader2: value2\r\n" "\r\n"))
(printn (string-split-string "header0: value0\r\nheader1: value1\r\nheader2: value2" "\r\n"))
(printn (string-split-string "header0: value0" "\r\n"))
(printn (string-split-string "header0: value0\r" "\r\n"))
(printn (string-split-string "header0: value0\r\n" "\r\n"))

(printn (string-replace-all "aaaaa---a---aaaaa---" "aaa" "b"))

