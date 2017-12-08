;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")
(include "lib/cmap.scm")
(include "lib/counter.scm")
(include "lib/parse/lexer.scm")
(include "lib/dfa/charset.scm")
(include "lib/dfa/rx.scm")
(include "lib/dfa/deriv.scm")
(include "lib/dfa/dfa.scm")
(include "lib/dfa/emit.scm")
(include "lib/dfa/lexicon.scm")

(if (< sys.argc 2)
    (begin (printf "\nGenerate a lexer dfa (in dot format) to stdout.\n\n")
           (printf "Usage: " sys.argv[0] " <lexicon.sg>\n")
           (printf "    example: $ " sys.argv[0] " sexp-lex.sg\n")
           (printf "  the input lexicon is in s-expression format.\n")
           -1)
    (let ((lexpath sys.argv[1])
          (lexfile (file/open-read lexpath))
          ;; read the parser spec as an s-expression
          (exp (reader lexpath (lambda () (file/read-char lexfile))))
          (lexicon (sexp->lexicon (car exp)))
          ;; convert the lexicon to a dfa
          (dfa0 (lexicon->dfa lexicon))
          ((labels dfa1) dfa0))
      (dfa->dot dfa1 sys.argv[1])
      0
      ))
