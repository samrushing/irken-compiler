;; -*- Mode: Irken -*-

;; build a lexer by converting a set of rxs to a DFA.
;;
;; to do this we need a submatch for each lexeme/category/kind of
;; token.  the resulting DFA may contain states that are final for
;; more than one lexeme.  we resolve the ambiguity by ordering the
;; lexemes, and choosing the highest priority (the earlier).

;; (list "thing") => (list (:tuple 'THING "thing"))
(define (make-keywords keywords)
  (map
   (lambda (word)
     ;; KIND word
     (:tuple (string->symbol (upcase word)) word))
   keywords))

;; XXX temporary workaround with compiler pvar literal issue #59
(define (make-one sym s)
  (:tuple sym s))

;; I think we need an rx-canon function.
(define build-or
  ()        -> (raise (:Lex/BogusOr))
  (a b)     -> (rx-or a b)
  (hd . tl) -> (rx-or hd (build-or tl))
  )

(define (lexicon->dfa lexicon)
  (let ((subs '())
        (i 0)
        (labels (alist:nil)))
    (for-list item lexicon
      (match item with
        (:tuple kind regex)
        -> (let (((rx pos) (p-rx regex 0)))
             (assert (= pos (string-length regex)))
             (alist/push labels i kind)
             (PUSH subs (rx:group i nullmatch rx))
             (set! i (+ i 1)))
        ))
    (let ((rx (build-or (reverse subs)))
          (dfa (rx->dfa rx)))
      ;;(printf "\nrx = " (pp-rx rx) "\n")
      ;;(print-dfa dfa)
      ;;(dfa->dot dfa "lexicon")
      (:tuple labels dfa)
      )
    ))

;; examine a final-state rx derivative, find any finalized
;;  submatches.  if there's more than one, report the one with the
;;  highest priority.
(define (test-for-final rx labels)
  (let ((fin (<< 1 30)))
    (define W
      (rx:cat a b)     -> (begin (W a) (W b))
      (rx:or a b)      -> (begin (W a) (W b))
      (rx:and a b)     -> (begin (W a) (W b))
      (rx:star a)      -> (W a)
      (rx:not a)       -> (W a)
      (rx:group n m a)
      -> (begin
           (if (and
                (eq? m.ms (mstate:final))
                (< n fin))
               (set! fin n))
           (W a))
      _ -> #u
      )
    (match rx with
      ;; aka rx-null: use special indicator for the sink state
      (rx:sym ()) -> '%%sink%%
      _ -> (begin
             (W rx)
             (if (< fin (<< 1 30))
                 (alist/lookup* labels fin 'not-final)
                 'not-final))
      )))

;; dfa = {
;;   map = (vector (list {ts=int sym=charset}))
;;   size = int
;;   finals = (set int)
;;   states = (list rx)
;; }

;; translate the deriv-dfa output to table form that can
;;   be used by lib/parse/lexer.scm: {step=(char int -> int) finals=(vector symbol)}

(define (dfa->lexer dfa0)
  (match dfa0 with
    (:tuple labels dfa1)
    -> (let ((utsvec (cmap/make magic-cmp))     ;; unique to-state vectors
             (tsvecs (make-vector dfa1.size 0)) ;; table of index into utsvec
             (finals (make-vector dfa1.size 'not-final))) ;;
         (for-range i dfa1.size
           (let ((tsvec (make-vector 256 -1)))
             ;; collect unique to-state vectors
             (for-list tran dfa1.map[i]
                (for-range j 256
                  (if (charset/in* tran.sym j)
                      (set! tsvec[j] tran.ts))))
             (set! tsvecs[i] (cmap/add utsvec tsvec))
             ;; collect finals
             (set! finals[i] (test-for-final (nth dfa1.states i) labels))))
         ;; sharing identical vectors can save some memory...
         ;; e.g. the C lexer has 178/239 unique vectors.
         (let ((table (make-vector dfa1.size #(0))))
           (for-range i dfa1.size
             (set! table[i] (cmap->item utsvec tsvecs[i]))
             ;; XXX identify sink states
             )
           ;; result: a step function and a vector of symbols
           {step=(lambda (ch state) (%array-ref #f table[state] (char->ascii ch)))
            table=table
            finals=finals
            })
         )))

;; rather than emitting the vector plainly: #(0 1 1 1 2 ...)
;; we emit a run-length-encoded version: (rle 0 (3 1) 2 ...)
;; this results in significantly smaller generated code.
(define (vector-as-rle v)

  (let ((r '())
        (last -1)
        (repeat 1))

    (define (emit val repeat)
      (PUSH
       r
       (if (= repeat 1)
           (format (int val))
           (format "(" (int repeat) " " (int val) ")"))))

    (for-vector val v
      (if (= val last)
          (set! repeat (+ repeat 1))
          (begin
            (emit last repeat)
            (set! last val)
            (set! repeat 1))))
    (emit last repeat)
    (format "(rle " (join " " (rest (reverse r))) ")")
    ))

;; code generation can be used in cases where you don't want to include
;; the rx/dfa machinery in your output.

(define (emit-irken-lexer dfa base)
  ;; dfa = {step table finals}
  (let ((size (vector-length dfa.table))
        (utsvec (cmap/make magic-cmp))
        (tsvecs (make-vector size 0)))
    (for-range i size
      (set! tsvecs[i] (cmap/add utsvec dfa.table[i])))
    (printf "(define table-" (sym base) "\n")
    (printf "  (let (\n")
    (for-map index tsvec utsvec.rev
      (printf "      (t" (int index) " " (vector-as-rle tsvec) ")\n"))
    (printf "        )\n")
    (printf "     (list->vector (LIST "
            (join (lambda (x) (format "t" (int x)))
                  " "
                  (vector->list tsvecs))
            "))))\n\n")
    ;; emit finals
    (printf "(define finals-" (sym base) " #(\n")
    (for-vector item dfa.finals
      (printf "  '" (sym item) "\n"))
    (printf "  ))\n\n")
    ;; emit step fun
    (printf "(define (step-" (sym base) " ch state)\n")
    (printf "  (%array-ref #f table-" (sym base) "[state] (char->ascii ch)))\n\n")
    (printf "(define dfa-" (sym base)
            " {step=step-" (sym base)
            " table=table-" (sym base)
            " finals=finals-" (sym base)
            "})\n")
    ))

;; make regex-safe literal
(define (rx-safe-string s)
  (define (safe ch)
    (if (member-eq? ch rx-metachars)
        (format (char #\\) (char ch))
        (format (char ch))))
  (string-concat (map safe (string->list s))))

;; (lexicon
;;   (INTEGER (reg "[0-9]+"))
;;   (IDENT   (reg "[A-Za-z]+"))
;;   (PLUS    (lit "+"))
;;   (CMP     (or (lit "<") (lit ">") ...))
;;   ...)

(define (sexp->lexicon exp)
  (define (upsym sym)
    (string->symbol (upcase (symbol->string sym))))

  (define sexp->item
    (sexp:list ((sexp:symbol 'lit) (sexp:string lit)))
    -> (rx-safe-string lit)
    (sexp:list ((sexp:symbol 'reg) (sexp:string reg)))
    -> reg
    (sexp:list ((sexp:symbol 'or) . items))
    -> (format (join "|" (map sexp->item items)))
    exp -> (raise (:Lexicon/Error "sexp->item" exp))
    )

  (define sexp->lexeme
    (sexp:list ((sexp:symbol kind) item))
    -> (:tuple (upsym kind) (sexp->item item))
    exp -> (raise (:Lexicon/Error "sexp->lexeme" exp))
    )
  (match exp with
    (sexp:list ((sexp:symbol 'lexicon) . rest))
    -> (map sexp->lexeme rest)
    exp -> (raise (:Lexicon/Error "sexp->lexicon" exp))
    ))

;; sample lexicons

(define t-lexicon
  (append (make-keywords (LIST "if" "then" "else"))
          (LIST
           (make-one 'IDENT "[A-Za-z_][A-Za-z_0-9]*")
           (make-one 'NUMBER "[0-9]+"))))

;; lexer for grammars
(define g-lexicon
  (LIST
   (make-one 'WHITESPACE "[ \n\t]+")
   (make-one 'COMMENT    "[ \t]*//[^\n]*\n")
   (make-one 'COLON      ":")
   (make-one 'VBAR       "\\|")
   (make-one 'SEMICOLON  ";")
   (make-one 'NAME       "[A-Za-z_][A-Za-z_0-9]*")
   (make-one 'NUMBER     "[0-9]+")))

;; lexer for C

;; note: this is a *nearly* complete C lexer.  Since this is used by
;; the FFI code, it leaves out unnecessary things like augmented
;; assignment (e.g. "+=").

(define (make-c-lexicon)

  (define c-keywords
    (LIST
     "auto" "break" "case" "char" "continue" "default" "do" "double" "else"
     "enum" "extern" "float" "for" "goto" "if" "int" "long" "register" "return"
     "short" "signed" "sizeof" "static" "struct" "switch" "typedef" "union"
     "unsigned" "void" "while"
     ))

  (define c-skipwords
    (LIST
     ;; clang
     "_Nullable" "_Nonnull" "_Null_unspecified"
     ;; C
     "const" "restrict" "volatile" "register" "_Atomic"
     ;; gcc
     "__extension__"
     ))

  (define (make-alt subs)
    (format (join "|" subs)))

  (define S rx-safe-string)

  (append
   (LIST
    (make-one 'WHITESPACE "[ \t\n]+")
    (make-one 'CPPLINE    "#[^\n]*\n")
    (make-one 'STRIP      (make-alt c-skipwords))
    (make-one 'ATTRIBUTE  "__attribute__"))
   (make-keywords c-keywords)
   (LIST
    (make-one 'IDENT      "[A-Za-z_][A-Za-z0-9_]*")
    (make-one 'NUMBER     "[0-9]+")     ;; needs float support
    (make-one 'STRING     "\"([^\n\"]|(\\.))*\"")
    (make-one 'CHAR       "'([^']+)'")
    (make-one 'COMP_OP    (make-alt (LIST "<" ">" "==" ">=" "<=" "<>" "!=")))
    (make-one 'PERCENT    (S "%"))
    (make-one 'AMPERSAND  (S "&"))
    (make-one 'AMPERSAND2 (S "&&"))
    (make-one 'LPAREN     (S "("))
    (make-one 'RPAREN     (S ")"))
    (make-one 'SPLAT      (S "*"))
    (make-one 'SPLATSPLAT (S "**"))
    (make-one 'PLUS       (S "+"))
    (make-one 'COMMA      (S ","))
    (make-one 'MINUS      (S "-"))
    (make-one 'DOT        (S "."))
    (make-one 'SLASH      (S "/"))
    (make-one 'COLON      (S ":"))
    (make-one 'SEMICOLON  (S ";"))
    (make-one 'LSHIFT     (S "<<"))
    (make-one 'EQUALS     (S "="))
    (make-one 'RSHIFT     (S ">>"))
    (make-one 'ATSIGN     (S "@"))
    (make-one 'LBRACKET   (S "["))
    (make-one 'RBRACKET   (S "]"))
    (make-one 'CARET      (S "^"))
    (make-one 'BACKQUOTE  (S "`"))
    (make-one 'LBRACE     (S "{"))
    (make-one 'VBAR       (S "|"))
    (make-one 'VBAR2      (S "||"))
    (make-one 'RBRACE     (S "}"))
    (make-one 'TILDE      (S "~"))
    (make-one 'QUESTION   (S "?"))
    (make-one 'DOTDOTDOT  (S "..."))
    )))
