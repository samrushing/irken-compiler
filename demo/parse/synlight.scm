;; -*- Mode: Irken -*-

;; syntax highlighting.

;; while this includes an irken-specific hack, it can be
;;  used with any lexer that uses the same token scheme.
;; [i.e., if you have a pascal lexer that generates
;;  COMMENT/STRING/etc... tokens it will work with this
;;  utility]
;;
;; at some point I'd like to design a markup system that
;;  lets you attach markup to grammar productions.

(include "lib/basis.scm")
(include "lib/getopt.scm")
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

;; ---------- ANSI terminal output ------------

;; 'bright' colors add 60 to each value
(define ansi-colors
  (alist/make
   ('black   30)
   ('red     31)
   ('green   32)
   ('yellow  33)
   ('blue    34)
   ('magenta 35)
   ('cyan    36)
   ('white   37)
   ))

(define ansi-color-table
  (alist/make
   ('COMMENT     'red)
   ('STRING      'magenta)
   ('KEYWORD     'blue)
   ('PRIVATE     'red)
   ('CONSTRUCTOR 'green)
   ('ARROW       'yellow)
   ('CHAR        'magenta)
   ('FUNCTION    'cyan)
   ))

(define CSI "\x1b[")

(define (ansi-markup s color)
  (match (alist/lookup* ansi-colors color -1) with
    -1  -> (format CSI "0m" s) ;; turn it all off
    n   -> (format CSI (int n) "m" s CSI "0m")
    ))

;; ---------- HTML output ------------

;; ... based on pygmentize output...
(define header "
<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01//EN\"
   \"http://www.w3.org/TR/html4/strict.dtd\">

<html>
<head>
  <title></title>
  <meta http-equiv=\"content-type\" content=\"text/html; charset=utf8\">
  <style type=\"text/css\">
td.linenos { background-color: #f0f0f0; padding-right: 10px; }
span.lineno { background-color: #f0f0f0; padding: 0 5px 0 5px; }
pre { line-height: 125%; }
body .hll { background-color: #ffffcc }
body  { background: #f8f8f8; }
body .c { color: #408080; font-style: italic } /* Comment */
body .err { border: 1px solid #FF0000 } /* Error */
body .k { color: #008000; font-weight: bold } /* Keyword */
body .s { color: #BA2121; } /* Literal.String */
body .p { color: #800000; } /* private */
body .t { color: #800080; } /* type */
body .f { color: #0000C0; font-weight:bold } /* function */
  </style>
</head>
<body>
<pre>
")

(define footer "</pre></div> </body> </html>")

(define css-color-table
  (alist/make
   ('COMMENT     'c)
   ('STRING      's)
   ('CONSTRUCTOR 't)
   ('KEYWORD     'k)
   ('ARROW       'k)
   ('PRIVATE     'p)
   ('CHAR        's)
   ('FUNCTION    'f)
   ))

(define html-escape-table
  (alist/make
   (#\< "&lt;")
   (#\> "&gt;")
   (#\& "&amp;")
   ))

(define (sanitize s)
  (define (escape ch)
    (match (alist/lookup html-escape-table ch) with
      (maybe:yes s) -> s
      (maybe:no)    -> (char->string ch)
      ))
  (string-concat (map escape (string->list s))))

(define html-markup
  s 'none -> (sanitize s)
  s class -> (format "<span class=\"" (sym class) "\">" (sanitize s) "</span>")
  )

;; can we annotate function/macro/datatype/etc names in a streaming fashion?
;; NOTE: this processor is specific to Irken, whereas without it any lexer
;;   using the same token classes can be used.  To do this correctly we need
;;   to base this utility on a grammar, not a lexer.

(define (annotate-functions gen)

  (let ((last (make-vector 4 "")))

    (define (is-fun? tok)
      (and (eq? tok.kind 'SYMBOL)
           (match last[0] last[1] last[2] last[3] with
             _ "(" "define" "(" -> #t
             _ _ "(" "define"   -> #t
             _ _ "(" "defmacro" -> #t
             _ _ "(" "datatype" -> #t
             _ _ _ _            -> #f
             )))

    (makegen emit
      (for tok gen
        (when (is-fun? tok)
          (set! tok.kind 'FUNCTION))
        (emit tok)
        (when (not (eq? tok.kind 'WHITESPACE))
          (set! last[0] last[1])
          (set! last[1] last[2])
          (set! last[2] last[3])
          (set! last[3] tok.val)
        )))
    ))

(define program-options
  (makeopt
   (flag 'h)    ;; help
   (flag 'html) ;; html output
   (flag 'ansi) ;; ansi output
   (pos 'lexer (string ""))
   (pos 'input (string ""))
   ))

(define (get-options)
  (if (< sys.argc 3)
      (usage)
      (try
       (process-options program-options (rest (vector->list sys.argv)))
       except
       (:Getopt/MissingArg _ _) -> (usage)
       (:Getopt/UnknownArg _ _) -> (usage)
       )))

(define (usage)
  (printf "\nSyntax Highlighter.\n\n")
  (printf "Usage: " sys.argv[0] " [-html|-ansi] <lexicon.sg> <input-file>\n")
  (printf "    example: $ " sys.argv[0] " irken-lex.sg demo/brainfuck.scm\n")
  (%exit #f -1))

(define (main)
  (let ((opts (get-options))
        (html? #f)
        (lexpath "")
        (input ""))
    (when-maybe b (get-bool-opt opts 'h)
      (usage))
    (when-maybe b (get-bool-opt opts 'html)
      (set! html? #t))
    (when-maybe b (get-bool-opt opts 'ansi)
      (set! html? #f))
    (when-maybe path (get-string-opt opts 'lexer)
      (set! lexpath path))
    (when-maybe path (get-string-opt opts 'input)
      (set! input path))
    (let ((lexfile (file/open-read lexpath))
          ;; read the parser spec as an s-expression
          (exp (reader lexpath (lambda () (file/read-char lexfile))))
          (lexicon (sexp->lexicon (car exp)))
          ;; convert the lexicon to a dfa
          (dfa0 (lexicon->dfa lexicon))
          ((labels dfa1) dfa0)
          ;; build a lexer from the dfa
          (lexer (dfa->lexer dfa0))
          ;; lex the given file
          (sfile (file/open-read input))
          (gen0 (file-char-generator sfile))
          (gen1 (make-lex-generator lexer gen0)))
      (when html?
        (printf header "\n"))
      (for tok (annotate-functions gen1)
        (if html?
            (printf (html-markup tok.val (alist/lookup* css-color-table tok.kind 'none)))
            (printf (ansi-markup tok.val (alist/lookup* ansi-color-table tok.kind 'none)))))
      (when html?
        (printf footer "\n"))
      )))

(main)
