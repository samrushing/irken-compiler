;; -*- Mode: Irken -*-

(require "lib/dfa/lexicon.scm")
(require "lib/parse/earley.scm")

;; This file supports putting an entire parser spec in a single file,
;;  in s-expression form.  The parser spec includes three components:
;;  The lexicon, the filter list, and the grammar.  It uses sexp->lexicon
;;  from lib/dfa/lexicon.scm.

;; read a parser from a .sg file.
;; ('sg' stands for 'sexp grammar')

(define (sexp->parser exp)

  ;; (filter name ...)

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
             (g0 (sexp->grammar grammar))
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

