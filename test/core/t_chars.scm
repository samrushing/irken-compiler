;; -*- Mode: Irken -*-

(include "lib/basis.scm")

(printn (list #\a #\Z #\0 #\space #\newline))
;; => (#\a #\Z #\0 #\space #\newline)

(printn (char->int #\A))
(printn (int->char 98))
(printn (char=? #\a #\a))
;; => 65
;; => #\b
;; => #t

(printn (list (digit? #\7) (digit? #\x) (letter? #\q) (whitespace? #\tab)))
;; => (#t #f #t #t)

(printn (list (toupper #\a) (tolower #\Q)))
(printn (list (upcase "MiXed") (downcase "MiXed")))
;; => (#\A #\q)
;; => ("MIXED" "mixed")
