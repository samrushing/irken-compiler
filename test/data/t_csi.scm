;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/csi.scm")

;; show escapes legibly.
(define (vis s)
  (let ((r (list:nil)))
    (for-string ch s
      (push! r (if (char=? ch #\u1b) "ESC" (char->string ch))))
    (string-concat (reverse r))))

(define (show-decode s)
  (match (csi-decode s) with
    (result:ok c)  -> (printf (sexp (csi->sexp c)) "\n")
    (result:err _) -> (printf "error\n")))

;; encoding
(printf (vis (csi->string (csi:el 2))) "\n")
;; => ESC[2K
(printf (vis (csi->string (csi:cup 3 7))) "\n")
;; => ESC[3;7H
(printf (vis (csi->string (csi:sgr (list SGR-bold (sgr-fg SGR-blue))))) "\n")
;; => ESC[1;34m
(printf (vis (format "<" (csi (csi:sgr (list 0))) ">")) "\n")
;; => <ESC[0m>

;; decoding: omitted params take their defaults.
(show-decode "\x1b[A")
;; => (cuu 1)
(show-decode "\x1b[5B")
;; => (cud 5)
(show-decode "\x1b[H")
;; => (cup 1 1)
(show-decode "\x1b[;5H")
;; => (cup 1 5)
(show-decode "\x1b[K")
;; => (el 0)
(show-decode "\x1b[2J")
;; => (ed 2)
(show-decode "\x1b[3S")
;; => (su 3)
(show-decode "\x1b[m")
;; => (sgr (0))
(show-decode "\x1b[1;34m")
;; => (sgr (1 34))
(show-decode "\x1b[6n")
;; => (dsr)
(show-decode "\x1b[24;80R")
;; => (ds 24 80)
(show-decode "\x1b[?25h")
;; => (other #\h (25))
(show-decode "\x1b[1~")
;; => (other #\~ (1))

;; round trip
(define (round-trip c)
  (match (csi-decode (csi->string c)) with
    (result:ok c0) -> (string=? (csi->string c0) (csi->string c))
    _ -> #f))
(printn (map round-trip (list (csi:cuu 4) (csi:cup 2 9) (csi:ed 1) (csi:sgr (list 1 31)) (csi:dsr))))
;; => (#t #t #t #t #t)
