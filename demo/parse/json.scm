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

;; this is a bit of a hack to get a parser included as a source file
;;  into a top-level variable containing the s-expression.

(defmacro parser
  (parser rest ...)
  -> (define json-parser
       ;; this puts the 'parser wrapper back around
       ;; the rest of the expression.
       (LIST (sexp1 'parser (%%sexp rest ...))))
  )

(include "json.sg")

;; datatype for JSON objects.

(datatype json
  (:string string)
  (:number int)
  (:bool bool)
  (:null)
  (:array (list json))
  (:obj (list (:tuple string json)))
  )

;; render a JSON object back into a string

(define pair-repr
  (:tuple s v) -> (format (string s) ":" (json-repr v))
  )

(define json-repr
  (json:string s)   -> (format (string s))
  (json:number n)   -> (format (int n))
  (json:bool #t)    -> "true"
  (json:bool #f)    -> "false"
  (json:null)       -> "null"
  (json:array vals) -> (format "[" (join json-repr ", " vals) "]")
  (json:obj pairs)  -> (format "{" (join pair-repr ", " pairs) "}")
  )

;; prety-print a json object.
(define (pp-json exp width)
  (define (recur d exp)
    (let ((repr (json-repr exp))
          (size (string-length repr)))
      (if (< size width)
	  (printf repr)
	  (match exp with
            (json:array vals)
            -> (begin
                 (printf "[")
                 (for-list val vals
                   (printf "\n" (repeat d "  "))
                   (recur (+ d 1) val))
                 (printf "]"))
            (json:obj pairs)
            -> (begin
                 (printf "{")
                 (for-list pair pairs
                   (match pair with
                     (:tuple key val)
                     -> (begin
                          (printf "\n" (repeat (+ d 1) "  ") (string key) " : ")
                          (recur (+ d 1) val))))
                 (printf "\n" (repeat d "  ") "}"))
            _ -> (printf repr)
            ))))
  (recur 0 exp)
  (printf "\n")
  )

;; hack to temporarily work around issue #59
(define (tuple a b)
  (:tuple a b))

;; ------------------------------------
;; build json object from a parse tree.
;; ------------------------------------

(define (parse-string s)
  (define esc
    acc (#\\ ch . rest)
    -> (esc (list:cons ch acc) rest)
    acc (#\")
    -> (list->string (reverse acc))
    acc (ch . rest)
    -> (esc (list:cons ch acc) rest)
    acc _
    -> (impossible)
    )
  (esc '() (rest (string->list s)))
  )

(define parse-pair
  (parse:nt pair ((parse:t string) COLON value))
  -> (tuple (parse-string string.val) (parse-value value))
  _ -> (impossible)
  )

(define parse-pairs
  (parse:nt 'pairs (pairs COMMA pair))
  -> (list:cons (parse-pair pair) (parse-pairs pairs))
  (parse:nt 'pairs (pair))
  -> (list:cons (parse-pair pair) (list:nil))
  _ -> (impossible)
  )

(define parse-values
  (parse:nt 'values (values COMMA value))
  -> (list:cons (parse-value value) (parse-values values))
  (parse:nt 'values (value))
  -> (list:cons (parse-value value) (list:nil))
  x -> (impossible)
  )

(define parse-value
  (parse:nt 'value ((parse:t tok)))
  -> (match tok.kind with
       'STRING -> (json:string (parse-string tok.val))
       'NUMBER -> (json:number (string->int tok.val))
       'TRUE   -> (json:bool #t)
       'FALSE  -> (json:bool #f)
       'NULL   -> (json:null)
       _       -> (impossible)
       )
  (parse:nt 'value ((parse:nt 'array (LBRACKET RBRACKET))))
  -> (json:array '())
  (parse:nt 'value ((parse:nt 'array (LBRACKET values RBRACKET))))
  -> (json:array (reverse (parse-values values)))
  (parse:nt 'value ((parse:nt 'obj (LBRACE pairs RBRACE))))
  -> (json:obj (reverse (parse-pairs pairs)))
  x -> (impossible)
  )

(define parse-json
  (parse:nt 'json (value)) -> (parse-value value)
  _ -> (impossible)
  )

(define (main)

  (let (((lexicon filter grammar start) (sexp->parser json-parser))
        ;; convert the lexicon to a dfa
        (dfa0 (lexicon->dfa lexicon))
        ((labels dfa1) dfa0)
        ;; build a lexer from the dfa
        (lexer (dfa->lexer dfa0))
        ;; create a stream of tokens
        (spath sys.argv[1])
        (sfile (file/open-read spath))
        (gen0 (make-lex-generator lexer sfile))
        ;; pass the stream through the token filter
        (gen1 (filter-gen filter gen0))
        ;; build it into a parse tree.
        (parse (earley grammar (prod:nt start) gen1)))
    ;;(pp (parse->sexp parse) 40)
    (let ((json (parse-json parse)))
      (pp-json json 50))
    ))

(main)
