;; -*- Mode: Irken -*-

(define (lex-grammar path)
  (make-generator
   (lambda (consumer)
     (define (emit tok)
       (consumer (maybe:yes tok)))
     (let ((file (file/open-read path))
           (gen0 (file-char-generator file))
           (gen1 (make-lex-generator dfa-g gen0))
           (toks '()))
       (for tok gen1
         (match tok.kind with
           'COMMENT    -> #u
           'WHITESPACE -> #u
           _           -> (emit tok))
         ))
     (forever (consumer (maybe:no)))
     )))

(define (parse-meta lexgen)
  ;; grammar is a mapping from prod -> (list (list prod))
  (let ((syntax    (prod:nt 'syntax))
        (rule      (prod:nt 'rule))
        (exp       (prod:nt 'exp))
        (seq       (prod:nt 'seq))
        (term      (prod:nt 'term))
        (COLON     (prod:t 'COLON))
        (VBAR      (prod:t 'VBAR))
        (NAME      (prod:t 'NAME))
        (SEMICOLON (prod:t 'SEMICOLON))
        (STRING    (prod:t 'STRING))
        (g (alist/make
            ('syntax (LIST (LIST syntax rule) (LIST rule)))
            ('rule   (LIST (LIST NAME COLON exp SEMICOLON)))
            ('exp    (LIST (LIST seq VBAR exp) (LIST seq)))
            ('seq    (LIST (LIST seq term) (LIST term)))
            ('term   (LIST (LIST STRING) (LIST NAME))))))
    (earley g syntax lexgen)))

(define (parse-error kind val)
  (raise (:ParseError (format "parse error: " (sym kind) " " (parse-repr val)))))

;; parse-xxx convert the parse tree into a grammar of the form
;;  seen above: an alist mapping prod -> (list (list prod))
(define parse-syntax
  (parse:nt 'syntax (rule))
  -> (let (((name prods) (parse-rule rule)))
       (alist:entry name prods (alist:nil)))
  (parse:nt 'syntax (syntax rule))
  -> (let (((name prods) (parse-rule rule)))
       (alist:entry name prods (parse-syntax syntax)))
  x -> (parse-error 'syntax x))

(define parse-rule
  (parse:nt 'rule ((parse:t NAME) COLON exp SEMICOLON))
  -> (:tuple (string->symbol NAME.val) (parse-exp exp))
  x -> (parse-error 'rule x))

(define parse-exp
  (parse:nt 'exp (seq))          -> (LIST (reverse (parse-seq seq)))
  (parse:nt 'exp (seq VBAR exp)) -> (list:cons (reverse (parse-seq seq)) (parse-exp exp))
  x -> (parse-error 'exp x))

(define parse-seq
  (parse:nt 'seq (term))     -> (LIST (parse-term term))
  (parse:nt 'seq (seq term)) -> (list:cons (parse-term term) (parse-seq seq))
  x -> (parse-error 'seq x))

(define parse-term
  (parse:nt 'term ((parse:t tok)))
  -> (if (upper? (string-ref tok.val 0))
         (prod:t (string->symbol tok.val))
         (prod:nt (string->symbol tok.val)))
  x -> (parse-error 'term x))

(define (print-grammar grammar)
  (printf "grammar: {\n")
  (alist/iterate
   (lambda (name alts)
     (let ((alts0 (map (lambda (x) (format (join prod-repr " " x))) alts)))
       (printf "  " (sym name) " : " (join " | " alts0) "\n")))
   grammar)
  (printf "}\n")
  )

(define (read-grammar path)
  (parse-syntax
   (parse-meta
    (lex-grammar path))))

(define (read-grammar-as-sexp path)
  (let ((parse (parse-meta (lex-grammar path))))
    (parse->sexp parse)))

;;(print-grammar (read-grammar sys.argv[1]))
