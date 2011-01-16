(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")
(include "lib/frb.scm")
(include "lib/symbol.scm")

(eq? 'thingy (string->symbol "thingy"))
(print the-symbol-table)
