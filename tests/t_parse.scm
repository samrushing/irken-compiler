
(include "lib/core.scm")
(include "lib/string.scm")
(include "lib/io.scm")
(include "lib/pair.scm")
(include "lib/vector.scm")
(include "lib/symbol.scm")
    
;; EOPLv1, chapter 11 

(define (parse tokens)

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

  (define (check/drop class next-action)
    (lambda (buffer token-seq)
      (let ((token (first token-seq)))
	(if (%eq? (token.class token) class)
	    (next-action buffer (next token-seq))
	    (syntax-error "check/drop" class token)))))
  
  (define (check/shift class next-action)
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

  (define (process-nt state next-action)
    (lambda (buffer token-seq)
      (let ((next-answer ((goto-parser-state state) '() token-seq)))
	(next-action (cons (answer.tree next-answer) buffer)
		     (answer.unparsed next-answer)))))

  (define (emit-list)
    (lambda (buffer token-seq)
      (make-answer (reverse buffer) token-seq)))


  (define (parse-once start-state token-seq)
    ((goto-parser-state start-state) '() token-seq))

  ;; the grammar

  (define (parse-command token)
    (let ((class (token.class token)))
      (cond ((%eq? class 'begin)
	     (check/drop
	      'begin
	      (process-nt
	       parse-command
	       (check/drop
		'semicolon
		(process-nt
		 parse-command
		 (check/drop
		  'end
		  (reduce 'compound-command)))))))
	    ((%eq? class 'variable)
	     (check/shift
	      'variable
	      (check/drop
	       'assign-sym
	       (process-nt
		parse-expression
		(reduce 'assignment-command)))))
	    (else (syntax-error "failed to parse command" #f token)))))

  (define (parse-expression token)
    (let ((class (token.class token)))
      (cond ((%eq? class 'variable)
	     (check/shift 'variable (reduce 'var-expression)))
	    ((%eq? class 'number)
	     (check/shift 'number (reduce 'constant-expression)))
	    ((%eq? class 'lparen)
	     (check/drop
	      'lparen
	      (process-nt
	       parse-expression
	       (check/drop
		'plus-sym
		(process-nt
		 parse-expression
		 (check/drop 
		  'rparen
		  (reduce 'sum-expression)))))))
	    (else (syntax-error "failed to parse expression" #f token)))))

  (parse-once parse-expression tokens)
  
  )

;'((variable . "x"))
(%printn (parse (cons (cons 'variable "x") '())))
;'((lparen . "(") (variable . "x") (plus-sym . "+") (variable . "y") (rparen . ")"))
(%printn (parse
	  (cons
	   (cons 'lparen "(")
	   (cons
	    (cons 'variable "x")
	    (cons
	     (cons 'plus-sym "+")
	     (cons
	      (cons 'variable "y")
	      (cons
	       (cons 'rparen ")")
	       '())))))))

		  
