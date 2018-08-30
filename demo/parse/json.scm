;; -*- Mode: Irken -*-

(require "lib/basis.scm")
(require "lib/cmap.scm")
(require "lib/counter.scm")
(require "lib/parse/lexer.scm")
(require "lib/dfa/charset.scm")
(require "lib/dfa/rx.scm")
(require "lib/dfa/deriv.scm")
(require "lib/dfa/dfa.scm")
(require "lib/dfa/emit.scm")
(require "lib/dfa/lexicon.scm")
(require "lib/parse/earley.scm")
(require "lib/parse/parser.scm")

(define json-parser
  (%%sexp
   (parser
    (lexicon
     (WHITESPACE (reg "[ \n\t]+"))
     ;; (STRING     (reg "\"([^\n\"\\\\]|\\\\.)*\""))
     ;; same expression, more readable:
     (STRING     (cat (lit "\"")
                      (* (or
                          ;; not newline, backslash, or quote
                          (not (or (lit "\n") (lit "\\") (lit "\"")))
                          ;; backslash + any character
                          (cat (lit "\\") (reg "."))))
                      (lit "\"")))
     (NUMBER     (reg "\\-?[0-9]+")) ;; XXX floating-point.
     (TRUE       (lit "true"))
     (FALSE      (lit "false"))
     (NULL       (lit "null"))
     (COLON      (lit ":"))
     (COMMA      (lit ","))
     (LBRACKET   (lit "["))
     (RBRACKET   (lit "]"))
     (LBRACE     (lit "{"))
     (RBRACE     (lit "}"))
     )
    (filter WHITESPACE COMMENT)
    (grammar
     (json   value)
     (obj    (LBRACE pairs RBRACE))
     (pairs  (pairs COMMA pair) pair)
     (pair   (STRING COLON value))
     (array  (LBRACKET values RBRACKET) (LBRACKET RBRACKET))
     (values (values COMMA value) value)
     (value  obj array STRING NUMBER TRUE FALSE NULL)
     ))
   ))

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

;; pretty-print a json object.

;; estimate the printed size of a json object
(define json-pp-pair-size
  (:tuple key val)
  -> (+ 5 (string-length key) (json-pp-size val)))

(define json-pp-size
  (json:bool #t)  -> 5
  (json:bool #f)  -> 6
  (json:null)     -> 4
  (json:number n) -> (string-length (int->string n))
  (json:string s) -> (+ 2 (string-length s))
  (json:obj pairs)
  -> (fold binary+ (* 2 (length pairs)) (map json-pp-pair-size pairs))
  (json:array vals)
  -> (fold binary+ (* 2 (length vals)) (map json-pp-size vals))
  )

(define (pp-json exp width)
  (define (recur d exp)
    (let ((size (json-pp-size exp)))
      (if (< size width)
	  (printf (json-repr exp))
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
            _ -> (printf (json-repr exp))
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
    acc (#\\ ch . rest) -> (esc (list:cons ch acc) rest)
    acc (#\")           -> (list->string (reverse acc))
    acc (ch . rest)     -> (esc (list:cons ch acc) rest)
    acc _               -> (impossible)
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
        (gen0 (file-char-generator sfile))
        (gen1 (make-lex-generator lexer gen0))
        ;; pass the stream through the token filter
        (gen2 (filter-gen filter gen1))
        ;; build it into a parse tree.
        (parse (earley grammar (prod:nt start) gen2)))
    ;;(pp (parse->sexp parse) 40)
    (let ((json (parse-json parse)))
      (pp-json json 50))
    ))

(main)
