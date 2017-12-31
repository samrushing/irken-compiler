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
   ))

(define CSI "\x1b[")

(define (ansi s color)
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
body .s { color: #BA2121 } /* Literal.String */
body .p { color: #800000; } /* private */
body .t { color: #B00040 } /* type */
  </style>
</head>
<body>
<pre>
")

(define footer "</pre></div> </body> </html>")

(define css-color-table
  (alist/make
   ('COMMENT 'c)
   ('STRING  's)
   ('CONSTRUCTOR 't)
   ('KEYWORD 'k)
   ('ARROW   'k)
   ('PRIVATE 'p)
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

(define (html s color)
  (format "<span class=\"" (sym color) "\">" (sanitize s) "</span>"))

(if (< sys.argc 3)
    (begin (printf "\nSyntax Highlighter.\n\n")
           (printf "Usage: " sys.argv[0] " <lexicon.sg> <input-file>\n")
           (printf "    example: $ " sys.argv[0] " irken-lex.sg demo/brainfuck.scm\n"))
    (let ((lexpath sys.argv[1])
          (lexfile (file/open-read lexpath))
          ;; read the parser spec as an s-expression
          (exp (reader lexpath (lambda () (file/read-char lexfile))))
          (lexicon (sexp->lexicon (car exp)))
          ;; convert the lexicon to a dfa
          (dfa0 (lexicon->dfa lexicon))
          ((labels dfa1) dfa0)
          ;; build a lexer from the dfa
          (lexer (dfa->lexer dfa0))
          ;; lex the given file
          (spath sys.argv[2])
          (sfile (file/open-read spath))
          (gen0 (file-char-generator sfile))
          (gen1 (make-lex-generator lexer gen0)))
      (printf header "\n")
      (for tok gen1
        ;;(printf (ansi tok.val (alist/lookup* ansi-color-table tok.kind 'none)))
        (printf (html tok.val (alist/lookup* css-color-table tok.kind 'none)))
        )
      (printf footer "\n")
      ))
