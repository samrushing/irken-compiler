;; create a bunch of symbols.

(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")
(include "lib/aa_map.scm")
(include "lib/symbol.scm")

'(not-final not-final whitespace not-final not-final comment newline)
;;	    not-final not-final string1 not-final not-final mulop augassign bitand
;;	    not-final not-final string2 not-final not-final lparen rparen mulop
;;	    power addop comma addop getattr mulop mulop number colon semicolon
;;	    compare shift compare assign compare compare compare shift ident ident
;;	    lbrace rbrace bitxor ident ident ident)
