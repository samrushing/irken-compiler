;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")
(include "lib/counter.scm")
(include "lib/dfa/charset.scm")
(include "lib/dfa/rx.scm")
(include "lib/dfa/deriv.scm")

(define (rx-apply regex s)
  (printf "regex: '" regex "'\n")
  (let ((rx (parse-rx regex)))
    (printf "parsed: " (rx-repr rx) "\n")
    (printf "parsed: " (pp-rx rx) "\n")
    (printf "trying '" s "'\n")
    (if (deriv-match rx s)
	(printf "accepted.\n")
	(printf "not accepted.\n"))
    ))

;(rx-apply "[A-Z][0-9]+" "M2112")
;(rx-apply ".*{[A-Z][0-9]+}" "M2112")
;(rx-apply "(a|a|b|a|b)+" "baab")
;(rx-repr (parse-rx "(a*|b*|a*|b*|a*)"))
;(rx-apply ".*{a+}" "aaabbaaaaaaa")
;(rx-apply "(((A)((B)((C)((D)((E)((F)(G)))))))(H))*" "ABCDEFGHABCDEFGH")
;(rx-apply "({A}|{AB}|{B})*" "ABABABABABABAB")
;(rx-apply ".*{a+b*a+}" "   aaabbaaa  aba ")
(rx-apply ".*{jim|jimbo}" " jim  jimbo ")
;(rx-apply ".*{a*|aaab}" " aaaaaaaaaaab ")
;(rx-apply ".*{carpark|funpark}" " fun car funpfunpark cacarpark ")
;(rx-apply ".*({[A-Z][a-z]*}{[0-9]+}xy" " Jim1934xy ")
;(rx-apply ".*{a+b*a+}c" "   aaabbaacaabaca ")

;; whoa this introduces waaaaay too many submatches, 
;;  need to grok why they are not being filtered.
;;  [this might be where the 'last * only' thing I keep
;;   seeing in the haskell papers shows up]
;(rx-apply ".*(({b+}|{a+})*c)" "   aaabbaacaabaca ")
;(rx-apply ".*{a+}*c" " aaaaacaaac  ")
