;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")
(include "lib/cmap.scm")
(include "lib/counter.scm")
(include "lib/set3.scm")
(include "lib/parse/lexer.scm")
(include "lib/dfa/charset.scm")
(include "lib/dfa/rx.scm")
(include "lib/dfa/deriv.scm")
(include "lib/dfa/dfa.scm")
(include "lib/dfa/emit.scm")
(include "lib/dfa/lexicon.scm")

(if (< sys.argc 3)
    (begin (printf "\nLex a file, given a lexicon.\n\n")
           (printf "Usage: " sys.argv[0] " <lexicon.sg> <input-file>\n")
           (printf "    example: $ " sys.argv[0] " sexp-lex.sg sexp-lex.sg\n"))
    (let ((lexpath sys.argv[1])
          (lexfile (file/open-read lexpath))
          ;; read the parser spec as an s-expression
          (exp (reader lexpath (lambda () (file/read-char lexfile))))
          (lexicon (sexp->lexicon (car exp)))
          ;; convert the lexicon to a dfa
          (dfa0 (lexicon->dfa lexicon))
          ((labels dfa1) dfa0)
          ;; build a lexer from the dfa
          (lexer (dfa->lexer dfa0))
          ;; lex the given file
          (spath sys.argv[2])
          (sfile (file/open-read spath))
          (gen0 (make-lex-generator lexer sfile)))
      (for tok gen0
        (printf (sym tok.kind) " " (string tok.val) "\n"))
      ))
