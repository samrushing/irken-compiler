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
(include "lib/parse/earley.scm")
(include "lib/parse/parser.scm")

(if (< sys.argc 3)
    (begin (printf "\nParse a file with a given grammar.\n")
           (printf " the output is an s-expression parse tree.\n\n")
           (printf "Usage: " sys.argv[0] " <grammar.sg> <input-file>\n\n")
           (printf "  example: $ " sys.argv[0] " bcpl.sg sample.bcpl\n")
           (printf "           $ " sys.argv[0] " meta.sg g.g\n\n")
           (printf "     <grammar>: an s-expression combined lexicon+grammar parser.\n")
           (printf "  <input-file>: a source file written in the target language.\n"))
    (let ((gpath sys.argv[1])
          (gfile (file/open-read gpath))
          ;; read the parser spec as an s-expression
          (exp (reader gpath (lambda () (file/read-char gfile))))
          ((lexicon filter grammar start) (sexp->parser exp))
          ;; convert the lexicon to a dfa
          (dfa0 (lexicon->dfa lexicon))
          ((labels dfa1) dfa0)
          ;; build a lexer from the dfa
          (lexer (dfa->lexer dfa0))
          ;; create a stream of tokens
          (spath sys.argv[2])
          (sfile (file/open-read spath))
          (gen0 (make-lex-generator lexer sfile))
          ;; pass the stream through the token filter
          (gen1 (filter-gen filter gen0))
          ;; build it into a parse tree.
          (parse (earley grammar (prod:nt start) gen1))
          )
      ;;(print-dfa dfa1)
      ;; pretty-print the parse tree
      (pp (parse->sexp parse) 40)
      ))
