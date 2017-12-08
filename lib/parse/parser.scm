;; -*- Mode: Irken -*-

;; This file supports putting an entire parser spec in a single file,
;;  in s-expression form.  The parser spec includes three components:
;;  The lexicon, the filter list, and the grammar.  It uses sexp->lexicon
;;  from lib/dfa/lexicon.scm.

;; read a parser from a .sg file.
;; ('sg' stands for 'sexp grammar')

(define (sexp->parser exp)

  ;; --------------------------------------------------
  ;; (grammar
  ;;   (e (e ADD t) t)
  ;;   (t (t MUL p) p)
  ;;   (p IDENT))

  (define p-term
    (sexp:symbol name)
    -> (if (upper? (string-ref (symbol->string name) 0))
           (prod:t name)
           (prod:nt name))
    exp -> (raise (:Parser/Error "p-term" exp))
    )

  (define p-alt
    (sexp:list terms)  -> (map p-term terms)
    (sexp:symbol term) -> (LIST (p-term (sexp:symbol term)))
    exp                -> (raise (:Parser/Error "p-alt" exp))
    )

  (define p-rule
    (sexp:list ((sexp:symbol nt) . alts))
    -> (:tuple nt (map p-alt alts))
    exp -> (raise (:Parser/Error "p-rule" exp))
    )

  (define p-grammar
    (sexp:list ((sexp:symbol 'grammar) . rules))
    -> (let ((rules0 (map p-rule rules)))
         (foldr (lambda (rule acc)
                  (match rule with
                    (:tuple name rule)
                    -> (alist:entry name rule acc)))
                (alist:nil)
                rules0))
    exp -> (raise (:Parser/Error "p-grammar" exp))
    )

  (define p-filter
    (sexp:symbol name)
    -> name
    exp -> (raise (:Parser/Error "p-filter" exp))
    )

  (define p-filters
    (sexp:list ((sexp:symbol 'filter) . names))
    -> (map p-filter names)
    exp -> (raise (:Parser/Error "p-filters" exp))
    )

  ;; --------------------------------------------------
  ;; (parser (lexicon ...) (filter ...) (grammar ...))

  (define p-parser
    ((sexp:list ((sexp:symbol 'parser) lexicon filters grammar)))
    -> (let ((l0 (sexp->lexicon lexicon))
             (f0 (p-filters filters))
             (g0 (p-grammar grammar))
             (s0 (start-symbol g0)))
         (:tuple l0 f0 g0 s0))
    exp -> (raise (:Parser/Error "p-parser" (sexp:list exp)))
    )

  ;; the start symbol is the first non-terminal.
  (define start-symbol
    (alist:entry start _ _)
    -> start
    (alist:nil)
    -> (raise (:Parser/Error "no grammar?" (sexp:symbol '?)))
    )

  (p-parser exp)
  )

(define (filter-gen symbols gen)
  (make-generator
   (lambda (consumer)
     (for tok gen
       (if (not (member-eq? tok.kind symbols))
           (consumer (maybe:yes tok))))
     (forever (consumer (maybe:no))))))

