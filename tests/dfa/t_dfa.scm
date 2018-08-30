;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")

(include "lib/cmap.scm")
(include "lib/counter.scm")
(include "lib/dfa/charset.scm")
(include "lib/dfa/rx.scm")
(include "lib/dfa/deriv.scm")
(include "lib/dfa/dfa.scm")
(include "lib/dfa/emit.scm")

(define (t0 regex)
  (let ((dfa (rx->dfa (parse-rx regex))))
    ;;(print-dfa dfa)
    (dfa->dot dfa regex)
    ))

;(t0 "a|ba|c")
;(t0 "ab|ac")
;(t0 "(a+b*a+)")
;(t0 "a+b")
;(t0 ".*{a+b*a+}")
;(t0 ".*{a*}b")
;(t0 ".*({[A-Z][a-z]*}{[0-9]+}xy")
;(t0 ".*({Aa*}x")

;; experimenting with using groups for a lexer
;;(t0 "{cat}|{dog}")
;;(t0 "{cat}|{cape}")
;;(t0 "{[A-Za-z][A-Za-z_]*}|{[ \t\n]+}|{[0-9]+}")
(t0 "{cat}|{dog}|{[A-Za-z][A-Za-z_]*}|{[0-9]+}")
;;(t0 "{a+}|{b+}")

