;; -*- Mode: Irken -*-

(include "lib/basis.scm")

(define s "hello, world")

(printn (string-length s))
(printn (string-ref s 4))
(printn (substring s 7 12))
;; => 12
;; => #\o
;; => "world"

(printn (string-append "foo" "bar" "baz"))
(printn (string-concat (list "a" "b" "c")))
(printn (string-join (list "x" "y" "z") ", "))
(printn (string-split "a:b::c" #\:))
;; => "foobarbaz"
;; => "abc"
;; => "x, y, z"
;; => ("a" "b" "" "c")

(printn (list (string->int "1234") (string->int "-56")))
(printn (list (int->string 789) (int->string -10)))
;; => (1234 -56)
;; => ("789" "-10")

(printn (list (string=? "abc" "abc") (string<? "abc" "abd") (string=? "abc" "ab")))
;; => (#t #t #f)

(printn (list (string-find "lo" s) (string-find "xyz" s)))
(printn (list (starts-with s "hell") (ends-with s "world")))
;; => (3 -1)
;; => (#t #t)

(printn (string->list "abc"))
(printn (list->string (list #\x #\y)))
;; => (#\a #\b #\c)
;; => "xy"

(printn (string-trim-both "  padded  "))
(printn (string-reverse "stressed"))
;; => "padded"
;; => "desserts"

;; strings are mutable
(define m (make-string 3))
(string-set! m 0 #\c)
(string-set! m 1 #\a)
(string-set! m 2 #\t)
(printn m)
;; => "cat"

;; printf / format
(printf "int " (int 42) " str " (string "q") " sym " (sym 'z) " bool " (bool #f) "\n")
(printn (format (lpad 5 (int 42)) "|" (rpad 4 "ab") "|" (zpad 4 (int 7)) "|" (hex 255)))
;; => int 42 str "q" sym z bool #f
;; => "   42|ab  |0007|ff"

;; quoting in repr: escapes
(printn "tab\there \"quoted\" back\\slash")
;; => "tab\x09here \"quoted\" back\\slash"

;; string-find restarts a failed partial match at the right place
(printn (list (string-find "llo" "hello") (string-find "aab" "aaab") (string-find "" "x")))
;; => (2 1 0)
