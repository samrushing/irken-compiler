
(include "lib/core.scm")
(include "lib/string.scm")
(include "lib/io.scm")
(include "lib/pair.scm")
(include "lib/vector.scm")
(include "lib/symbol.scm")

(define (make-list-generator list)
  (make-generator
   (lambda (consumer)
     (let inner ()
       (let loop ((l list))
	 (let ((again #f))
	   (cond ((null? l)
		  (consumer '(end-of-list "end-of-list")))
		 (else
		  (print-string "CONSUME")
		  (consumer (car l))
		  (loop (cdr l))))))))))

(define (parse filename)

  (let* ((token-gen (make-list-generator '(,(cons 'number "1") ,(cons 'addop "+") ,(cons 'number "2") ,(cons 'compare ">") ,(cons 'number "7"))))
	 (current-token #f))

    (define (syntax-error expected got)
      (print-string "syntax error, expected <")
      (%print expected)
      (print-string "> but got ")
      (%print got)
      (print-string "\n")
      (error "parse failed"))
    
    (define (token.class tok) (car tok))
    
    (define (token.data tok) (cdr tok))
    
     (define (next-token)
       ;; filter/process the token stream
       (let loop ((tok (token-gen)))
 	(%printn tok)
 	(case (token.class tok)
 	  ((whitespace)
 	   (loop (token-gen)))
 	  ((comment)
 	   (loop (token-gen)))
 	  (else tok))))

    (define (token) current-token)
    (define (next)
      (set! current-token (next-token))
      ;;(set! current-token (token-gen))
      (print-string "next => ") (%printn current-token))
    
    (define (match tok class data)
      (and (eq? class (token.class tok))
	   (string-=? data (token.data tok))))

    ;; atom: ('(' [yield_expr|testlist_gexp] ')' | '[' [listmaker] ']' | '{' [dictmaker] '}' | '`' testlist1 '`' | NAME | NUMBER | STRING+)

    (define (p-atom2)
      (let ((tok (token)))
	(print-string "p-atom ") (%printn tok)
	(case (token.class tok)
	  ((ident)
	   (next)
	   (cons 'varref (token.data tok)))
	  ((number)
	   (next)
	   (cons 'int (token.data tok)))
	  ((string1 string2)
	   (next)
	   (cons 'string (token.data tok)))
	  (else
	   (syntax-error 'atom tok)))))

    (define (p-atom)
      (let ((r (p-atom2)))
	(print-string "p-atom returned ")
	(%printn r)
	r))

    ;; power: atom trailer* ['**' factor]
    ;; trailer: '(' [arglist] ')' | '[' subscriptlist ']' | '.' NAME
    ;; factor: ('+'|'-'|'~') factor | power

    ;; term: factor (('*'|'/'|'%'|'//') factor)*
    ;; arith_expr: term (('+'|'-') term)*
    ;; shift_expr: arith_expr (('<<'|'>>') arith_expr)*
    ;; or_test: and_test ('or' and_test)*
    ;; and_test: not_test ('and' not_test)*
    ;; not_test: 'not' not_test | comparison
    ;; comparison: expr (comp_op expr)*
    ;; comp_op: '<'|'>'|'=='|'>='|'<='|'<>'|'!='|'in'|'not' 'in'|'is'|'is' 'not'
    ;; expr: xor_expr ('|' xor_expr)*
    ;; xor_expr: and_expr ('^' and_expr)*
    ;; and_expr: shift_expr ('&' shift_expr)*
    ;; shift_expr: arith_expr (('<<'|'>>') arith_expr)*

    (define (p-binop class p)
      (print-string "binop ") (%printn class)
      (let loop ((n1 (p))
		 (tok (token)))
	(print-string "loop tok=") (%print class) (%print tok) (%printn n1)
	(cond ((%eq? (token.class tok) class)
	       (next)
	       (%printn "about to call (p)")
	       (loop #(binop ,(token.data tok) ,n1 ,(p)) (token)))
	      (else
	       (print-string "binop returning ") (%print class) (%printn n1)
	       n1))))

    (define (p-term)
      (p-binop 'mulop p-atom))
    (define (p-arith)
      (p-binop 'addop p-term))
    (define (p-shift)
      (p-binop 'shift p-arith))
    (define (p-and)
      (p-binop 'and p-shift))
    (define (p-xor)
      (p-binop 'xor p-and))
    (define (p-expr)
      (p-binop 'or p-xor))
    (define (p-compare)
      (p-binop 'compare p-expr))
    
    (define (p-is)
      (let ((n1 (p-compare)))
	(cond ((match (token) 'ident "is")
	       (next)
	       (cond ((match (token) 'ident "not")
		      #(binop "is-not" ,n1 ,(p-compare)))
		     (else
		      #(binop "is" ,n1 ,(p-compare)))))
	      (else n1))))
    
    (define (p-in)
      (let ((n1 (p-is))
	    (tok (token)))
	(cond ((match tok 'ident "in")
	       (next)
	       #(binop "in" ,n1 ,(p-is)))
	      ((match tok 'ident "not")
	       (next)
	       (cond ((match (token) 'ident "in")
		      #(binop "not-in" ,n1 ,(p-is)))
		     (else
		      #(binop "in" ,n1 ,(p-is)))))
	      (else n1))))
    
    ;;(fill-tokens)
    ;;(%printn tokens)
    (next)
    ;;(p-in)
    ;;(p-arith)
    (p-compare)

    ))

;; (let ((g (make-lex-generator "tests/example.py")))
;;   (%printn (g))
;;   (%printn (g))
;;   (%printn (g))
;;   )

(parse "tests/example.py")

	   

