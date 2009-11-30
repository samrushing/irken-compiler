(include "lib/core.scm")
(include "lib/frb.scm")
(include "lib/pair.scm")
(include "lib/string.scm")
(include "lib/symbol.scm")

(printn (string-length "howdyx"))
(printn (string-compare "howdy" "howdyx"))
(printn (string-compare "howdyx" "howdy"))
(printn (string-compare "abc" "defghijkl"))
(printn (string-compare "defghijkl" "abc"))

(printn (string->symbol "howdy"))
(printn (string->uninterned-symbol "howdy"))
(printn (string->symbol "testing"))
(printn #('florb 'testing 'grok 'urk))

(list->string (string->list "sam"))
