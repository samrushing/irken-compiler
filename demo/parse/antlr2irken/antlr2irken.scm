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

;; translate an antlr grammar into an irken sexp grammar.

;; ---- lexer & grammar ----

;; this is not a *full* antlr parser.  my goal here is to cover enough
;; syntax to handle the C11 grammar.
(define antlr-parser
  (%%sexp
   (parser
    (lexicon
     (COMMENT    (or
                  (cat (lit "//")
                       (* (not (lit "\n")))
                       (lit "\n"))
                  (cat (lit "/*")
                       (* (or (cat (lit "*") (not (lit "/")))
                              (not (lit "*"))))
                       (cat (lit "*/")))))
     (STRING     (cat (lit "'")
                      (* (or
                          (not (or (lit "\n") (lit "\\") (lit "'")))
                          (cat (lit "\\") (reg "."))))
                      (lit "'")))
     (CHARSET    (cat (lit "[")
                      (* (or
                          (not (or (lit "\\") (lit "]")))
                          (cat (lit "\\") (reg "."))))
                      (lit "]")))
     (GRAMMAR    (lit "grammar"))
     (FRAGMENT   (lit "fragment"))
     (SKIP       (lit "skip"))
     (NAME       (reg "[A-Za-z_][A-Za-z0-9_]*"))
     (WHITESPACE (reg "[ \n\t]+"))
     (COLON      (lit ":"))
     (SEMICOLON  (lit ";"))
     (VBAR       (lit "|"))
     (STAR       (lit "*"))
     (QUESTION   (lit "?"))
     (PLUS       (lit "+"))
     (TILDE      (lit "~"))
     (LPAREN     (lit "("))
     (RPAREN     (lit ")"))
     (ARROW      (lit "->"))
     (DOT        (lit "."))
     )
    (filter WHITESPACE COMMENT)
    (grammar
     (gram   (gname rules))
     (gname  (GRAMMAR NAME SEMICOLON))
     (rules  (rules rule) rule)
     (rule   (NAME COLON alts SEMICOLON)
             (FRAGMENT NAME COLON alts SEMICOLON)
             (NAME COLON alts ARROW SKIP SEMICOLON))
     (alts   (alts VBAR seq) (seq VBAR) seq)
     (seq    (seq ebn0) ebn0)
     (ebn0   (TILDE ebn) ebn)
     (ebn    (LPAREN alts RPAREN)
             (ebn PLUS)
             (ebn STAR)
             (ebn QUESTION)
             term)
     (term   STRING NAME CHARSET DOT)
     )
    )))

;; ---- data structure ----

(datatype exp
  (:literal string)
  (:name symbol)
  (:charset string)
  (:cat (list exp))
  (:or (list exp))
  (:not exp)
  (:* exp)
  (:+ exp)
  (:? exp)
  )

(define canon-exp
  (exp:or (one))        -> (canon-exp one)
  (exp:cat (one))       -> (canon-exp one)
  (exp:or exps)         -> (exp:or (map canon-exp exps))
  (exp:cat exps)        -> (exp:cat (map canon-exp exps))
  (exp:* exp)           -> (exp:* (canon-exp exp))
  ;; push NOT down into charsets (ANTLR seems to prefer ~[Xx]* to [^Xx]* ??)
  (exp:not (exp:* exp)) -> (exp:* (canon-exp (exp:not exp)))
  (exp:not (exp:+ exp)) -> (exp:+ (canon-exp (exp:not exp)))
  (exp:not (exp:? exp)) -> (exp:? (canon-exp (exp:not exp)))
  (exp:not exp)         -> (exp:not (canon-exp exp))
  (exp:+ exp)           -> (exp:+ (canon-exp exp))
  (exp:? exp)           -> (exp:? (canon-exp exp))
  otherwise             -> otherwise
  )

(datatype rule
  (:parse symbol exp)
  (:lex symbol exp)
  (:fragment symbol exp)
  (:skip symbol exp)
  )

(define exp->sexp
  (exp:literal s) -> (sexp (sym 'literal) (string s))
  (exp:name s)    -> (sexp (sym 'name) (sym s))
  (exp:charset s) -> (sexp (sym 'charset) (string s))
  (exp:cat exps)  -> (sexp1 'cat (map exp->sexp exps))
  (exp:or exps)   -> (sexp1 'or (map exp->sexp exps))
  (exp:not exp)   -> (sexp (sym 'not) (exp->sexp exp))
  (exp:* exp)     -> (sexp (sym '*) (exp->sexp exp))
  (exp:+ exp)     -> (sexp (sym '+) (exp->sexp exp))
  (exp:? exp)     -> (sexp (sym '?) (exp->sexp exp))
  )

(define (exp-repr exp)
  (repr (exp->sexp exp)))

(define rule->sexp
  (rule:parse name exp)
  -> (sexp (sym 'rule) (sym name) (exp->sexp exp))
  (rule:lex name exp)
  -> (sexp (sym 'lex-rule) (sym name) (exp->sexp exp))
  (rule:fragment name exp)
  -> (sexp (sym 'lex-fragment) (sym name) (exp->sexp exp))
  (rule:skip name exp)
  -> (sexp (sym 'skip) (sym name) (exp->sexp exp))
  )

(define (rule-repr r)
  (repr (rule->sexp r)))

;; ---- parse the ANTLR AST ----

(define (p-error x)
  (raise (:ANTLR2Irken/ParseError x)))

(define p-grammar
  (parse:nt 'gram ((parse:nt 'gname (GRAMMAR (parse:t NAME) SEMICOLON)) rules))
  -> (:tuple NAME.val (p-rules rules))
  x -> (p-error x)
  )

(define p-rules
  (parse:nt 'rules (rules rule))
  -> (append (p-rules rules) (list (p-rule rule)))
  (parse:nt 'rules (rule))
  -> (list:cons (p-rule rule) (list:nil))
  x -> (p-error x)
  )

(define (is-lex? s)
  (upper? (string-ref s 0)))

(define p-rule
  (parse:nt 'rule ((parse:t NAME) COLON alts SEMICOLON))
  -> (if (is-lex? NAME.val)
         (rule:lex (string->symbol NAME.val) (canon-exp (exp:or (p-alts alts))))
         (rule:parse (string->symbol NAME.val) (canon-exp (exp:or (p-alts alts)))))
  (parse:nt 'rule (FRAGMENT (parse:t NAME) COLON alts SEMICOLON))
  -> (rule:fragment (string->symbol NAME.val) (canon-exp (exp:or (p-alts alts))))
  (parse:nt 'rule ((parse:t NAME) COLON alts ARROW SKIP SEMICOLON))
  -> (rule:skip (string->symbol NAME.val) (canon-exp (exp:or (p-alts alts))))
  x -> (p-error x)
  )

(define p-alts
  (parse:nt 'alts (alts VBAR seq))
  -> (list:cons (exp:cat (p-seq seq)) (p-alts alts))
  (parse:nt 'alts (seq VBAR)) ;; empty rule
  -> (list:cons (exp:cat (p-seq seq)) (list:nil))
  (parse:nt 'alts (seq))
  -> (list:cons (exp:cat (p-seq seq)) (list:nil))
  x -> (p-error x)
  )

(define p-seq
  (parse:nt 'seq (seq ebn0))
  -> (append (p-seq seq) (list (p-ebn0 ebn0)))
  (parse:nt 'seq (ebn0))
  -> (list (p-ebn0 ebn0))
  x -> (p-error x)
  )

(define p-ebn0
  (parse:nt 'ebn0 (TILDE ebn))
  -> (exp:not (p-ebn ebn))
  (parse:nt 'ebn0 (ebn))
  -> (p-ebn ebn)
  x -> (p-error x)
  )

(define p-ebn
  (parse:nt 'ebn (LPAREN alts RPAREN))
  -> (exp:or (p-alts alts))
  (parse:nt 'ebn (ebn (parse:t postfix)))
  -> (match postfix.kind with
       'PLUS -> (exp:+ (p-ebn ebn))
       'STAR -> (exp:* (p-ebn ebn))
       'QUESTION -> (exp:? (p-ebn ebn))
       x -> (p-error (parse:t postfix)))
  (parse:nt 'ebn (term))
  -> (p-term term)
  x -> (p-error x)
  )

(define (remove-quotes s)
  (let ((slen (string-length s)))
    (substring s 1 (- slen 1))))

(define p-term
  (parse:nt 'term ((parse:t term)))
  -> (match term.kind with
       'STRING  -> (exp:literal (remove-quotes term.val))
       'NAME    -> (exp:name (string->symbol term.val))
       'CHARSET -> (exp:charset term.val)
       'DOT     -> (exp:charset ".")
       x        -> (p-error (parse:t term)))
  x -> (p-error x)
  )

;; ---- translation ----

;; grammar translation.
;;
;; we need to convert rules with ?/*/+ operators to rules without operators for our earley parser.
;;
;; '?': we can convert this by adding two rules: one with, one without the subexp.
;; '+': add a new 'list-style' production rule for this subexp. (i.e., (rule (things thing) thing))
;; '*': convert to (? (+ x))
;;
;; complications:
;; 1) it would be nice to recognize when the same component is
;;    operated on in the same way and use the same rule. [done]
;; 2) recognize when we are operating on a lexer rule and just push
;;    that operator down into the regex. [this could be in another pass]
;;
;; (r (a b? c? d))
;; => (r (a b c? d))
;;    (r (a c? d))
;; now when those rules are seen:
;; (r (a b c? d))
;; => (r (a b d))
;;    (r (a b c d))
;; and
;; (r (a c? d))
;; => (r (a d))
;;    (r (a c d))
;;
;; for a final result of:
;;
;; (r (a b d))
;; (r (a b c d))
;; (r (a d))
;; (r (a c d))

;; XXX need to check for 'completely nullable' rules and raise an error.
(define (convert-rule name alts)
  (let ((rule-map (cmap/make magic-cmp))
        (new-rules '()))

    ;; we replace a 'plus' expression with a new rule.
    ;; each new rule is named via the counting-map
    (define (make-plus-rule name exp)
      ;; i.e. (thing-rule (things thing) thing)
      (rule:parse
       name
       (exp:or
        (list
         (exp:cat
          (list (exp:name name) exp))
         exp)
        )))

    (define (make-or-rule name exp)
      (rule:parse name exp))

    (define (make-cat-rule name exp)
      (rule:parse name exp))

    (define (replace-rule exp make)
      (let ((index (cmap/add rule-map exp))
            (new-name (string->symbol (format (sym name) "-" (int index)))))
        (push! new-rules (make new-name exp))
        (exp:name new-name)))

    (define walk-exp
      p (exp:cat es) -> (exp:cat (map p es))
      p (exp:or es)  -> (exp:or (map p es))
      p (exp:not e)  -> (exp:not (p e))
      p (exp:* e)    -> (exp:* (p e))
      p (exp:+ e)    -> (exp:+ (p e))
      p (exp:? e)    -> (exp:? (p e))
      p e            -> e
      )

    ;; replace any embedded (X Y) or (X | Y) with a new rule.
    (define (replace-subs exp)
      (define R
        (exp:cat es) -> (replace-rule (exp:cat es) make-cat-rule)
        (exp:or es)  -> (replace-rule (exp:or es) make-or-rule)
        otherwise    -> otherwise
        )
      (walk-exp R exp)
      )

    ;; translate * and + into ? and new-rule.
    (define translate-operators
      (exp:* e)    -> (exp:? (translate-operators (exp:+ e)))
      (exp:+ e)    -> (replace-rule e make-plus-rule)
      exp          -> exp
      )

    (define (expand-nullables exps)
      (define maybe-exp
        (exp:? e) -> (list (maybe:no) (maybe:yes e))
        exp       -> (list (maybe:yes exp))
        )
      (define recur
        ()        -> (list '())
        (hd . tl) -> (let ((hd0 (maybe-exp hd)) ;; (list (maybe exp))
                           (tl0 (recur tl))     ;; (list (list exp))
                           (r '()))
                       ;; similar to lib/combinatorics.scm::product
                       (for-list mhi hd0
                         (for-list tli tl0
                           (match mhi with
                             (maybe:yes hi) -> (push! r (list:cons hi tli))
                             (maybe:no)     -> (push! r tli)
                             )))
                       ;; (printf "recur: => ") (printn r)
                       (reverse r)))

      (define maybe-cat
        (x) -> x
        xs  -> (exp:cat xs)
        )

      (map maybe-cat (recur exps)))

    ;; replace exp with a list of exp (implied cat)
    (define remove-outer-cat
      (exp:cat es) -> es
      exp          -> (list exp)
      )

    (define (process-alt exp)
      (let ((items0 (remove-outer-cat exp))
            (items1 (map replace-subs items0))
            (items2 (map translate-operators items1)))
        (expand-nullables items2)))

    ;; body of convert-rule.
    (let ((new-alts '()))
      (for-list alt alts
        (for-list alt0 (process-alt alt)
          (push! new-alts alt0)))
      (:tuple (reverse new-alts) (reverse new-rules))
      )))

(define (convert-rules rules)
  (define exp->alts
    (exp:or alts) -> alts
    exp           -> (list exp)
    )
  (let ((fifo (queue/make))
        (result '()))
    (for-list rule rules
      (queue/add! fifo rule))
    (while-queue rule fifo
      (match rule with
        (rule:parse name exp)
        -> (let (((new-alts new-rules) (convert-rule name (exp->alts exp))))
             (push! result (rule:parse name (exp:or new-alts)))
             (for-list new-rule new-rules
               (queue/add! fifo new-rule)))
        other
        -> #u
        ))
    (reverse result)
    ))

(define (read-antlr-string s)
  (let loop ((acc '())
             (chars (string->list s)))
    (match chars with
      () -> (list->string (reverse acc))
      (#\\ x . tl)
      -> (match x with
           #\n -> (loop (list:cons #\newline acc) tl)
           #\r -> (loop (list:cons #\return acc) tl)
           #\t -> (loop (list:cons #\tab acc) tl)
           #\' -> (loop (list:cons #\' acc) tl)
           #\\ -> (loop (list:cons #\\ acc) tl)
           _   -> (raise (:ANTLR2Irken/BadString s)))
      (hd . tl)
      -> (loop (list:cons hd acc) tl)
      )))

(define (lit->rx s)
  (let ((chars (string->list (read-antlr-string s)))
        (r '()))
    (for-list ch chars
      (push! r (rx:sym (charset/single (char->int ch)))))
    (nary->binary rx:cat (reverse r))))

(define (cs->rx s)
  ;; (printf "cs->rx " (string s) "\n")
  (if (string=? s ".")
      (rx-sym charset/dot)
      (let ((s0 (substring s 1 (- (string-length s) 1))) ;; strip brackets
            (s1 (read-antlr-string s0)) ;; interpret backslash escapes
            (cs (parse-charset s1)))
        (rx-sym cs))))

(define (collect-lex-rules rules)

  (let ((rmap (tree/empty))
        (rxmap (tree/empty)))

    (define (get-rx name)
      (match (tree/member rxmap symbol-index-cmp name) with
        (maybe:yes rx) -> rx
        (maybe:no)
        -> (match (tree/member rmap symbol-index-cmp name) with
             (maybe:yes rule)
             -> (let ((rx (lex2rx rule)))
                  (tree/insert! rxmap symbol-index-cmp name rx)
                  rx)
             (maybe:no)
             -> (raise (:ANTLR2Irken/UnknownLexRule name))
             )
        ))

    (define lex2rx
      (exp:charset set)   -> (cs->rx set)
      (exp:literal s)     -> (lit->rx s)
      (exp:cat (a b))     -> (rx-cat (lex2rx a) (lex2rx b))
      (exp:cat (hd . tl)) -> (rx-cat (lex2rx hd) (lex2rx (exp:cat tl)))
      (exp:or (a b))      -> (rx-or (lex2rx a) (lex2rx b))
      (exp:or (hd . tl))  -> (rx-or (lex2rx hd) (lex2rx (exp:or tl)))
      (exp:cat _)         -> (raise (:ANTLR2Irken/Lex2Rx))
      (exp:or _)          -> (raise (:ANTLR2Irken/Lex2Rx))
      (exp:* a)           -> (rx-star (lex2rx a))
      (exp:not a)         -> (rx-not (lex2rx a))
      (exp:+ a)           -> (rx-plus (lex2rx a))
      (exp:? a)           -> (rx-optional (lex2rx a))
      (exp:name name)     -> (get-rx name)
      )

    ;; first pass, build rmap
    (for-list rule rules
      (match rule with
        (rule:lex name exp)      -> (tree/insert! rmap symbol-index-cmp name exp)
        (rule:fragment name exp) -> (tree/insert! rmap symbol-index-cmp name exp)
        (rule:skip name exp)     -> (tree/insert! rmap symbol-index-cmp name exp)
        _ -> #u
        ))
    ;; second pass, convert to rx
    (for-list rule rules
      (match rule with
        (rule:lex name exp)      -> (tree/insert! rxmap symbol-index-cmp name (lex2rx exp))
        (rule:fragment name exp) -> (tree/insert! rxmap symbol-index-cmp name (lex2rx exp))
        (rule:skip name exp)     -> (tree/insert! rxmap symbol-index-cmp name (lex2rx exp))
        _ -> #u
        ))
    rxmap
    ))

(define lit-rx-counter 0)

(define (rule->earley rrxmap rule)

  (define (replace-rx rx)
    (match (tree/member rrxmap magic-cmp rx) with
      (maybe:yes name)
      -> (sexp:symbol name)
      (maybe:no)
      -> (let ((count lit-rx-counter)
               ;; XXX we can do a better job of naming these.
               (name (string->symbol (format "LIT_" (int count)))))
           (inc! lit-rx-counter)
           (tree/insert! rrxmap magic-cmp rx name)
           (sexp:symbol name))
      ))

  (define (convert-or-lit exps)
    (let ((lits '()))
      (for-list exp exps
        (match exp with
          (exp:literal lit)
          -> (push! lits (lit->rx lit))
          _ -> (raise (:ANTLR2Irken/UnexpectedGrammarItem (rule-repr rule)))
          ))
      (nary->binary rx:or lits)))

  (define (convert-not exp)
    (match exp with
      (exp:or lits) -> (rx-not (convert-or-lit lits))
      _ -> (raise (:ANTLR2Irken/UnexpectedGrammarItem (repr (exp->sexp exp))))
      ))

  (define W
    (exp:literal s) -> (replace-rx (lit->rx s))
    (exp:name s)    -> (sexp:symbol s)
    (exp:cat exps)  -> (sexp:list (map W exps))
    ;; these should only happen with regular expressions
    (exp:or exps)   -> (replace-rx (convert-or-lit exps))
    (exp:not exp)   -> (replace-rx (convert-not exp))
    _ -> (raise (:ANTLR2Irken/UnexpectedGrammarItem (rule-repr rule)))
    )

  (let ((new-rule
         (match rule with
           (rule:parse name (exp:or alts))
           -> (sexp1 name (map W alts))
           (rule:parse name alt) ;; single alt
           -> (sexp (sym name) (W alt))
           _
           -> (raise (:ANTLR2Irken/UnexpectedRule (rule-repr rule))))))
    (:tuple rrxmap new-rule)
    ))

(define (collect-used-terminals terminals erules)
  (define W
    (sexp:symbol name)
    -> (when (upper? (string-ref (symbol->string name) 0))
         (set/insert! terminals symbol-index-cmp name))
    (sexp:list subs)
    -> (for-each W subs)
    e -> (raise (:ANTLR2Irken/UnexpectedEarleyRule (repr e)))
    )
  (for-list erule erules
    (match erule with
      (sexp:list ((sexp:symbol rname) . alts))
      -> (for-each W alts)
      _ -> (impossible)
      ))
  terminals
  )

(define (collect-skip-terminals rules)
  (let ((terminals (set/empty)))
    (for-list rule rules
      (match rule with
        (rule:skip name _)
        -> (set/insert! terminals symbol-index-cmp name)
        _ -> #u))
    terminals))

(define (print-rules rules)
  (for-list rule rules
    (pp (rule->sexp rule) 80)
    ))

(define (print-parse parse)
  (pp (parse->sexp parse) 80))

(define (main)
  (let (((lexicon filter grammar start) (sexp->parser antlr-parser))
        (dfa0 (lexicon->dfa lexicon))
        ((labels dfa1) dfa0)
        (lexer (dfa->lexer dfa0))
        (spath sys.argv[1])
        (sfile (file/open-read spath))
        (gen0 (file-char-generator sfile))
        (gen1 (make-lex-generator lexer gen0))
        (gen2 (filter-gen filter gen1))
        (parse (earley grammar (prod:nt start) gen2))
        (_ (print-parse parse))
        ((name rules) (p-grammar parse))
        (_ (print-rules rules))
        (rxmap (collect-lex-rules rules))
        (rrxmap (tree/empty))
        (used-terminals (collect-skip-terminals rules)))
    (for-map name rx rxmap
      (tree/insert! rrxmap magic-cmp rx name))
    (let ((new-rules (convert-rules rules))
          (earley-rules '())
          (lexicon '()))
      (for-list rule new-rules
        (let (((rrxmap0 earley-rule) (rule->earley rrxmap rule)))
          (push! earley-rules earley-rule)
          (set! rrxmap rrxmap0)))
      (set! used-terminals (collect-used-terminals used-terminals earley-rules))
      (for-map rx name rrxmap
        (when (set/member? used-terminals symbol-index-cmp name)
          (push! lexicon (sexp (sym name) (rx->sexp rx)))))
      (printf ";; -*- Mode: Lisp -*-\n")
      (pp (sexp (sym 'parser)
                (sexp1 'lexicon (reverse lexicon))
                (sexp (sym 'filter))
                (sexp1 'grammar (reverse earley-rules)))
          100)
      )))

(main)
