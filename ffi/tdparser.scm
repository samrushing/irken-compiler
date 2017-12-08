;; -*- Mode: Irken -*-

(include "parse/gparser.scm")
(include "parse/clextab.scm")

;; scan for typedefs, structs, and function declarations in
;;   preprocessed C output.

;; partition a file into a series of top-level objects.  this is done
;;   by reading declarations that are terminated by semicolons.  In
;;   order to do this 'safely', we must count the bracket/paren indent
;;   level and only consider a semicolon a delimiter when it occurs at
;;   the outermost level.

(define (partition-stream gen)
  (make-generator
   (lambda (consumer)
     (define (emit ob)
       (consumer (maybe:yes ob)))
     (let ((plevel 0)
           (blevel 0)
           (acc '()))
       (define (add tok)
         (PUSH acc tok))
       (for tok gen
         (match blevel plevel tok.kind with
           ;; these are various tokens we ignore
           _ _ 'WHITESPACE -> #u
           _ _ 'COMMENT    -> #u
           _ _ 'CPPLINE    -> #u
           _ _ 'SKIP       -> #u
           ;; a ';' at the outermost level means we need
           ;; to flush the current 'declaration'.
           0 0 'SEMICOLON
           -> (begin
                (add tok)
                (add {kind='eof val="" range=tok.range})
                (emit (reverse acc))
                (set! acc '()))
           ;; maintain paren and brace level
           n _ 'LBRACE -> (begin (add tok) (set! blevel (+ blevel 1)))
           n _ 'RBRACE -> (begin (add tok) (set! blevel (- blevel 1)))
           _ n 'LPAREN -> (begin (add tok) (set! plevel (+ plevel 1)))
           _ n 'RPAREN -> (begin (add tok) (set! plevel (- plevel 1)))
           _ _ _ -> (add tok)
           ))
       (emit (reverse acc))
       (forever (consumer (maybe:no)))
       ))))

(define (toks->string toks)
  (format ";; " (join (lambda (x) x.val) " " toks) "\n"))

(define (is-tok? tok kind)
  (eq? tok.kind kind))

(if (< sys.argc 3)
    (begin (printf "\nScan a .cpp (pre-processed C) file for typedefs.\n\n")
           (printf "Usage: " sys.argv[0] " c.g file.c\n")
           (printf "example: $ clang -E /usr/include/zlib.h > /tmp/zlib.cpp\n")
           (printf "         $ " sys.argv[0] " c.g /tmp/zlib.cpp\n"))
    (let ((tdgram (read-grammar sys.argv[1]))
          (file (file/open-read sys.argv[2]))
          (gen0 (make-lex-generator dfa-c file))
          (gen1 (partition-stream gen0)))
      (for toks gen1
        (printf (toks->string toks) "\n")
        (if (is-tok? (car toks) 'TYPEDEF)
            (try
             (pp (parse->sexp
                  (earley tdgram (prod:nt 'typedef)
                          (list-generator toks))) 40)
             except (:NoParse tok)
             -> (printf "unable to parse: " (toks->string toks))
             )
            ))))
