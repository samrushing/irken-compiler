
(include "lib/core.scm")
(include "lib/string.scm")
(include "lib/io.scm")
(include "lib/pair.scm")
(include "lib/vector.scm")

(define (int-in-range x lo hi)
  ;; range-test integer <x> in [lo,hi)
  (%%verify "TC_INT" 1 x)
  (%%verify "TC_INT" 1 lo)
  (%%verify "TC_INT" 1 hi)
  (%%cexp "(((%s >= %s) && (%s < %s)) ? PXLL_TRUE : PXLL_FALSE)" x lo x hi))

(define (lex filename)

  (let ((fd (open filename 0))
	(state 0)
	(current '())
	(tokens '())
	(final #f)
	(last-final #f)
	(finals #f)
	)

      (define (push-token)
	(print-string "PUSH TOKEN ")
	(%printn last-final)
	(print-string "  : ") (%print last-final) (print-string "\n")
	(set! tokens (cons (cons last-final (list->string (reverse current))) tokens))
	(print-string (list->string (reverse current)))
	(set! current '())
	(set! last-final #f)
	(set! state 0)
	)

      ;; defines the <step> function (DFA) from the lexer generator
      (include "tests/step.scm")

      ;; initialize the table of final states
      (set! finals (gen-finals))

      (let loop ((s (read fd 10000)))
	(%printn (string-length s))
	(if (%zero? (string-length s))
	    (begin (print-string "done?") #t)
	    (let loop2 ((n 0))
	      (print-string "[") (print-int n) (print-string ":") (%print last-final) (%print final) (print-string "]")
	      (if (%== n (string-length s))
		  (loop (read fd 1024))
		  (let ((ch (string-ref s n)))
		    (set! state (step (char->ascii ch) state))
		    (set! final (vector-ref finals state))
		    (%print ch) (print-string "->") (print-int state) (print-string " ")
		    (if (and last-final (not final))
			;; transition out
			(begin
			  (push-token)
			  (loop2 n)
			  )
			(begin
			  (set! last-final final)
			  (set! current (cons ch current))
			  (loop2 (%+ n 1)))))))))
      (if last-final
	  (push-token))
      (reverse tokens)
      ))

(define (parse)

  (let ((s (lex "example.py")))

  ;; tokens as a list of (<kind>, <data>) pairs
  ;; --------------------------------------------------
  (define (first tokens)
    (car tokens))
  
  (define (next tokens)
    (cdr tokens))

  (define (token.class token)
    (car token))

  (define (token.data token)
    (cdr token))
  ;; --------------------------------------------------

  ;; why not use cons???
  (define (make-answer tree unparsed)
    #(tree unparsed))
  
  (define (answer.tree a)
    (vector-ref a 0))

  (define (answer.unparsed a)
    (vector-ref a 1))

  ;; --------------------------------------------------
  (define (syntax-error msg expected got)
    (%print "syntax error ")
    (%print msg)
    (%printn expected)
    (%print got)
    (error "parse failed"))

  (define (drop class next-action)
    (lambda (buffer token-seq)
      (let ((token (first token-seq)))
	(if (%eq? (token.class token) class)
	    (next-action buffer (next token-seq))
	    (syntax-error "check/drop" class token)))))
  
  (define (shift class next-action)
    (lambda (buffer token-seq)
      (let ((token (first token-seq)))
	(if (%eq? (token.class token) class)
	    (next-action (cons (token.data token) buffer) (next token-seq))
	    (syntax-error "check/shift" class token)))))
  
  (define (reduce prod-name)
    (lambda (buffer token-seq)
      (make-answer
       (cons prod-name (reverse buffer))
       token-seq)))

  (define (goto-parser-state state)
    (lambda (buffer token-seq)
      (let ((next-action (state (first token-seq))))
	(next-action buffer token-seq))))

  (define (NT state next-action)
    (lambda (buffer token-seq)
      (let ((next-answer ((goto-parser-state state) '() token-seq)))
	(next-action (cons (answer.tree next-answer) buffer)
		     (answer.unparsed next-answer)))))

  (define (emit-list)
    (lambda (buffer token-seq)
      (make-answer (reverse buffer) token-seq)))

  (define (parse-once start-state token-seq)
    ((goto-parser-state start-state) '() token-seq))

  (define (is-class tok class)
    (%eq? (token.class tok) class))

  (define (p-atom tok)
      (cond ((is-class tok 'ident)
	     (shift 'ident (reduce 'varref)))
	    ((is-class tok 'number)
	     (shift 'number (reduce 'literal)))
	    ((is-class tok 'string1)
	     (shift 'string1 (reduce 'string)))
	    ((is-class tok 'string2)
	     (shift 'string2 (reduce 'string)))
	    (else (syntax-error 'atom tok))))
  
  (define (p-trailer tok)
    ;; trailer: '(' [arglist] ')' | '[' subscriptlist ']' | '.' NAME
    (cond ((is-token tok 'lparen)
	   
	   (drop 'lparen (NT p-arglist (
	  