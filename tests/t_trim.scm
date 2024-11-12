;; -*- Mode: Irken -*-


(require "lib/basis.scm")

(printf (string (string-trim-left "    \tthing  ")) "\n")
(printf (string (string-trim-right "\t\tthing\t\t\t")) "\n")
(printf (string (string-trim-both "\t\tthing\t\t\t")) "\n")
(printf (string (string-trim-left "  x")) "\n")
(printf (string (string-trim-left "  ")) "\n")
(printf (string (string-trim-left "")) "\n")
(printf (string (string-trim-right "x  ")) "\n")
(printf (string (string-trim-right "  ")) "\n")
(printf (string (string-trim-right "")) "\n")
(printf (string (string-trim-both " x ")) "\n")
(printf (string (string-trim-both "  ")) "\n")
(printf (string (string-trim-both "")) "\n")
(printf (string (string-trim-both " a b c d ")) "\n")
(printf (string (string-trim-both "a b c d                ")) "\n")

