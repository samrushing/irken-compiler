
(include "lib/core.scm")
(include "lib/string.scm")
(include "lib/io.scm")
(include "lib/pair.scm")
(include "lib/vector.scm")
(include "lib/symbol.scm")

(define (lex filename consumer)

  (let ((fd (open filename 0))
	(current '())
	(final #f)
	(last-final #f)
	)

      (define (build-token)
	(cons last-final (list->string (reverse current))))

      ;; defines the <step> function (DFA) from the lexer generator
      (include "tests/step5.scm")

      (let block-loop ((s (read fd 10000))
		       (state 0)
		       (slen 0)
		       (ch #f)
		       )
	(set! slen (string-length s))
	(if (= slen 0)
	    (if last-final (consumer (build-token)) #f)
	    (let char-loop ((i 0))
	      (cond ((= i slen)
		     (block-loop (read fd 1024) state))
		    (else
		     (set! ch (string-ref s i))
		     (set! state (step ch state))
		     (set! final (vector-ref finals state))
		     (cond ((and last-final (not final))
			    ;; transition out
			    (consumer (build-token))
			    (set! current '())
			    (set! last-final #f)
			    (set! state 0)
			    (char-loop i))
			   (else
			    (set! last-final final)
			    (set! current (cons ch current))
			    (char-loop (+ i 1))))))
	      )))
      ))

(define (make-lex-generator filename)
  (make-generator
   (lambda (consumer)
     (lex filename consumer)
     (let forever ()
       (consumer '(end-of-file . end-of-file))
       (forever))
     )))

(define (parse filename)

  (let* ((token-gen (make-lex-generator filename))
	 (current-token #f)
	 (start-of-line #t)
	 (saved-data #f)
	 )

    (define (syntax-error expected got)
      (print-string "syntax error, expected <")
      (%print expected)
      (print-string "> but got ")
      (%print got)
      (print-string "\n")
      (error "parse failed"))
    
    (define (token.class tok) (car tok))
    
    (define (token.data tok) (cdr tok))
    
    (define (rename-punctuation ch)
      ;; this may be temporary - by having each of these be a separate
      ;;   final state, the generated step function was getting a bit
      ;;   unwieldy.  since we'll probably convert the thing to a table-
      ;;   driven lexer eventually, this could go away...
      (case ch
	((#\.) 'getattr)
	((#\=) 'assign)
	((#\:) 'colon)
	((#\,) 'comma)
	((#\() 'lparen)
	((#\)) 'rparen)
	((#\[) 'lbrace)
	((#\]) 'rbrace)
	((#\{) 'lbracket)
	((#\}) 'rbracket)
	((#\&) 'bitand)
	((#\|) 'bitor)
	((#\^) 'bitxor)
	((#\~) 'bitnot)
	((#\;) 'semicolon)
	))

    (define (get-indent tok)
      (cond ((match tok 'whitespace)
	     ;; XXX handle or disallow tabs
	     (string-length (token.data tok)))
	     ;; non-whitespace at the front of a line
	    (else 0)))

    (define next-token
      ;; process (i.e., filter/synthesize) the token stream
      (let ((paren-stack '())
	    (indentation 0)
	    (start-of-line #t)
	    (held-token #f)
	    (tok #f)
	    )
	(lambda ()
	  (let loop ()
	    ;; I'm *not* happy with this.  I feel like the whole
	    ;;   indent/dedent thing can be done more cleanly with
	    ;;   either another wrapper function or a generator...
	    (cond (held-token
		   (set! tok held-token)
		   (set! held-token #f))
		  (else
		   (set! tok (token-gen))))
	    (if start-of-line
		;; in this state we might emit INDENT/DEDENT
		(let ((this-indent (get-indent tok)))
		  (set! start-of-line #f)
		  (set! held-token tok)
		  (cond ((> this-indent indentation)
			 (set! indentation this-indent)
			 (cons 'indent 'indent))
			((< this-indent indentation)
			 (set! indentation this-indent)
			 (cons 'dedent 'dedent))
			(else
			 (loop))))
		(case (token.class tok)
		  ((newline)
		   (cond ((null? paren-stack)
			  (set! start-of-line #t)
			  (cons 'newline 'newline))
			 (else (loop))))
		  ((whitespace comment) (loop))
		  ((keyword)
		   ;; convert keywords into their own 'class'
		   ;;  XXX downcase!
		   (let ((keyword (string->symbol (token.data tok))))
		     (cons keyword keyword)))
		  ((punctuation)
		   ;; convert punctuation into their own 'class'
		   (let ((punc (rename-punctuation (string-ref (token.data tok) 0))))
		     (cons punc punc)))
		  (else tok)))))))

    (define (token) current-token)

    (define (next)
      (set! current-token (next-token))
      (set! start-of-line (eq? (token.class current-token) 'newline))
      )

    (define (match tok class)
      (eq? class (token.class tok)))

    (define (match-string/drop tok class data)
      (cond ((and (match tok class)
		  (string-=? data (token.data tok)))
	     (next)
	     #t)
	    (else #f)))

    (define (match/drop tok class)
      (cond ((match tok class)
	     (next)
	     #t)
	    (else #f)))

    (define (match/save tok class)
      (cond ((match tok class)
	     (set! saved-data (token.data tok))
	     (next)
	     #t)
	    (else #f)))

    (define (expect class yes-proc error?)
      (if (match/drop (token) class)
	  (yes-proc)
	  (if error?
	      (syntax-error class (token))
	      #f)))

    (define (expect2 c1 c2 yes-proc error?)
      (if (match/drop (token) c1)
	  (expect c2 yes-proc #t)
	  (if error?
	      (syntax-error c1 (token))
	      #f)))

    (define (expect/data class error?)
      (cond ((match (token) class)
	     (let ((data (token.data (token))))
	       (next)
	       data))
	    (error?
	     (syntax-error class (token)))
	    (else #f)))

    ;; >>> taking inspiration from Pyrex's recursive-descent parser <<<

    ;; atom: ('(' [yield_expr|testlist_gexp] ')' | '[' [listmaker] ']' | '{' [dictmaker] '}' | '`' testlist1 '`' | NAME | NUMBER | STRING+)

    (define (p-atom)
      (let ((tok (token)))
	(case (token.class tok)
	  ((ident)
	   (next)
	   #(varref ,(token.data tok)))
	  ((number)
	   (next)
	   #(int ,(token.data tok)))
	  ((string1 string2)
	   ;; this should really be STRING+
	   (next)
	   #(string ,(token.data tok)))
	  ((lbrace)
	   (next)
	   #(list ,(p-listmaker)))
	  ((lbracket)
	   (next)
	   #(dict ,(p-dictmaker)))
	  (else
	   (syntax-error 'atom tok)))))

    ;; listmaker: test ( list_for | (',' test)* [','] )
    ;; XXX not doing <list_for> for now
    (define (p-listmaker)
      (let loop ((list (cons (p-test) '())))
	(if (match/drop (token) 'rbrace)
	    list
	    (if (match/drop (token) 'comma)
		(loop (cons (p-test) list))
		(syntax-error 'comma (token))))))

    ;;dictmaker: test ':' test (',' test ':' test)* [',']
    (define (p-dictpair)
      (let ((n1 (p-test)))
	(if (match/drop (token) 'colon)
	    (cons n1 (p-test))
	    (syntax-error 'colon (token)))))

    (define (p-dictmaker)
      (let loop ((list (cons (p-dictpair) '())))
	(if (match/drop (token) 'rbracket)
	    list
	    (if (match/drop (token) 'comma)
		(loop (cons (p-dictpair) list))
		(syntax-error 'comma (token))))))

    ;; trailer: '(' [arglist] ')' | '[' subscriptlist ']' | '.' NAME
    (define (p-trailer atom)
      (let loop ((trailers (cons atom '())))
	(cond ((match/drop (token) 'lparen)
	       (loop (cons #(call ,(p-arglist)) trailers)))
	      ((match/drop (token) 'lbrace)
	       (loop (cons (p-subscript) trailers)))
	      ((match/drop (token) 'getattr)
	       (if (match (token) 'ident)
		   (let ((name (token.data (token))))
		     (next)
		     (loop (cons #(getattr ,name) trailers)))
		   (syntax-error 'ident (token))))
	      (else trailers))))

    ;; power: atom trailer* ['**' factor]
    (define (p-power)
      (let* ((atom (p-atom))
	     (tok (token)))
	(case (token.class (token))
	  ((lparen lbrace getattr)
	   (p-trailer atom))
	  ((power)
	   (next)
	   #(binop power ,atom ,(p-factor)))
	  (else
	   atom))))

    ;; factor: ('+'|'-'|'~') factor | power
    (define (p-factor)
      (let ((tok (token)))
	(cond ((match-string/drop tok 'addop "+")
	       (p-factor))
	      ((match-string/drop tok 'addop "-")
	       #(unary-minus ,(p-factor)))
	      ((match/drop (token) 'bitnot)
	       #(bitnot ,(p-factor)))
	      (else
	       (p-power)))))

    ;; helper for binary operators
    (define (p-binop class p)
      (let loop ((n1 (p))
		 (tok (token)))
	(cond ((match/drop tok class)
	       (loop #(binop ,(token.data tok) ,n1 ,(p)) (token)))
	      (else n1))))

    ;; term: factor (('*'|'/'|'%'|'//') factor)*
    (define (p-term)
      (p-binop 'mulop p-factor))
    ;; arith_expr: term (('+'|'-') term)*
    (define (p-arith)
      (p-binop 'addop p-term))
    ;; shift_expr: arith_expr (('<<'|'>>') arith_expr)*
    (define (p-shift)
      (p-binop 'shift p-arith))
    ;; and_expr: shift_expr ('&' shift_expr)*
    (define (p-and)
      (p-binop 'bitand p-shift))
    ;; xor_expr: and_expr ('^' and_expr)*
    (define (p-xor)
      (p-binop 'bitxor p-and))
    ;; expr: xor_expr ('|' xor_expr)*
    (define (p-expr)
      (p-binop 'bitor p-xor))

    ;; comp_op: '<'|'>'|'=='|'>='|'<='|'<>'|'!='|'in'|'not' 'in'|'is'|'is' 'not'
    ;; comparison: expr (comp_op expr)*

    (define (p-compare)
      (p-binop 'compare p-expr))
    
    (define (p-is)
      (let ((n1 (p-compare)))
	(cond ((match/drop (token) 'is)
	       (cond ((match/drop (token) 'not)
		      #(binop "is-not" ,n1 ,(p-compare)))
		     (else
		      #(binop "is" ,n1 ,(p-compare)))))
	      (else n1))))
    
    (define (p-in)
      (let ((n1 (p-is))
	    (tok (token)))
	(cond ((match/drop tok 'in)
	       #(binop "in" ,n1 ,(p-is)))
	      ((match/drop tok 'not)
	       (cond ((match/drop (token) 'in)
		      #(binop "not-in" ,n1 ,(p-is)))
		     (else
		      #(binop "in" ,n1 ,(p-is)))))
	      (else n1))))

    ;; not_test: 'not' not_test | comparison
    (define (p-not-test)
      (if (match/drop (token) 'not)
	  #(not ,(p-not-test))
	  (p-in)))

    ;; and_test: not_test ('and' not_test)*
    (define (p-and-test)
      (let ((n1 (p-not-test)))
	(cond ((match/drop (token) 'and)
	       #(and ,n1 ,(p-not-test)))
	      (else n1))))

    ;; or_test: and_test ('or' and_test)*
    (define (p-or-test)
      (let ((n1 (p-and-test)))
	(cond ((match/drop (token) 'or)
	       #(or ,n1 ,(p-and-test)))
	      (else n1))))

    ;; test: or_test ['if' or_test 'else' test] | lambdef
    (define (p-test)
      (if (match/drop (token) 'lambda)
	  (p-lambdef)
	  (let ((n1 (p-or-test)))
	    (if (match/drop (token) 'if)
		(let ((n2 (p-or-test)))
		  (if (match/drop (token) 'else)
		      #(test ,n1 ,n2 ,(p-test))
		      (syntax-error 'else (token))))
		n1))))
		
    ;; gonna fake this for now, maybe get rid of it
    (define (p-old-test)
      (p-test))

    ;; lambdef: 'lambda' [varargslist] ':' test
    (define (p-lambdef)
      (error "not yet"))

    ;; listmaker: test ( list_for | (',' test)* [','] )

    ;; exprlist: expr (',' expr)* [',']
    ;; XXX This is nasty - how do we know when the list is over?
    ;;   if you look at the grammar, exprlist is followed by 'in'
    ;;   in most places.  delexpr is the exception.  figure this out.
    (define (p-exprlist)
      (let loop ((exprs '()))
	(let ((expr (p-expr)))
	  (cond ((match/drop (token) 'comma)
		 (loop (cons expr exprs)))
		((%eq? (token.class (token)) 'in)
		 ;; we're done
		 exprs)
		(else
		 (syntax-error 'exprlist (token)))))))

    ;; gen_iter: gen_for | gen_if
    ;; gen_for: 'for' exprlist 'in' or_test [gen_iter]
    ;; gen_if: 'if' old_test [gen_iter]

    ;; these three p-gen-xxx funs act more like probes than normal
    ;;   parsing functions - they will return #f rather than raising
    ;;   a syntax error...
    (define (p-gen-for)
      (if (match/drop (token) 'for)
	  (let ((exprlist (p-exprlist)))
	    (if (match/drop (token) 'in)
		(let* ((or-test (p-or-test))
		       (gen-iter-probe (p-gen-iter)))
		  (if gen-iter-probe
		      #(gen-for ,exprlist ,or-test ,gen-iter-probe)
		      #(gen-for ,exprlist ,or-test)))
		(syntax-error 'in (token))))
	  #f))

    (define (p-gen-if)
      (if (match/drop (token) 'if)
	  (let ((old-test (p-old-test))
		(gen-iter-probe (p-gen-iter)))
	    (if gen-iter-probe
		#(gen-if ,old-test ,gen-iter-probe)
		#(gen-if ,old-test)))
	  #f))

    (define (p-gen-iter)
      (case (token.class (token))
	((for) (p-gen-for))
	((if) (p-gen-if))
	(else #f)))

    ;; list_iter: list_for | list_if
    ;; list_for: 'for' exprlist 'in' testlist_safe [list_iter]
    ;; list_if: 'if' old_test [list_iter]

    ;; argument: test [gen_for] | test '=' test  # Really [keyword '='] test
    (define (p-argument)
      (let ((n1 (p-test)))
	(cond ((match/drop (token) 'assign)
	       #(keyword ,n1 ,(p-test)))
	      (else
	       (let ((gen-for-probe (p-gen-for)))
		 (if gen-for-probe
		     #(gen ,n1)
		     n1))))))

    ;; arglist: (argument ',')* (argument [',']| '*' test [',' '**' test] | '**' test)
    ;; Note: we'll verify correct ordering outside of the parser...
    (define (p-arglist)
      (let loop ((args '()))
	(cond ((match/drop (token) 'rparen)
	       args)
	      ((match-string/drop (token) 'mulop "*")
	       (loop (cons #(restargs ,(p-argument)) args)))
	      ((match/drop (token) 'power)
	       (loop (cons #(keyargs  ,(p-argument)) args)))
	      ((match/drop (token) 'comma)
	       (loop args))
	      (else
	       (loop (cons (p-argument) args))))))

    ;; actual Python grammar
    ;; -----------------------
    ;; subscriptlist: subscript (',' subscript)* [',']
    ;; subscript: '.' '.' '.' | test | [test] ':' [test] [sliceop]
    ;; sliceop: ':' [test]

    ;; what we'll do For Now
    ;; ---------------------
    ;; subscriptlist: subscript
    ;; subscript: [test] ':' [test]

    (define (p-subscript)
      (let ((n1 (if (match/drop (token) 'colon) #f (p-test))))
	(let ((n2 (if (match (token) 'rbrace) #f (p-test))))
	  #(slice ,n1 ,n2))))

    ;; ================================================================================

    ;; stmt: simple_stmt | compound_stmt
    ;; simple_stmt: small_stmt (';' small_stmt)* [';'] NEWLINE
    
    ;; how about we just 'inline' testlist here?
    ;; testlist: test (',' test)* [',']
    
    (define (p-testlist)
      (let loop ((list '()))
	(case (token.class (token))
	  ;; these terminate a 'testlist'
	  ((assign colon rbracket rbrace rparen augassign newline)
	   ;; XXX detect >1 element, emit with #(tuple) wrapping...
	   list)
	  ;; this continues one (maybe)
	  ((comma)
	   (next)
	   (loop list))
	  (else
	   (loop (cons (p-test) list))))))

    (define (p-delimited p separator)
      ;; parse a list of <p> delimited by <separator>
      (let loop ((result (cons (p) '())))
	(if (match/drop (token) separator)
	    (loop (cons (p) result))
	    result)))

    (define (p-yield-or-testlist)
      (if (match/drop (token) 'yield)
	  #(yield ,(p-testlist))
	  (p-testlist)))

    ;; expr_stmt: testlist (augassign (yield_expr|testlist) | ('=' (yield_expr|testlist))*)
    ;; augassign: ('+=' | '-=' | '*=' | '/=' | '%=' | '&=' | '|=' | '^=' | '<<=' | '>>=' | '**=' | '//=')
    (define (p-expr-stmt)
      (let ((n1 (p-testlist)))
	(cond ((match/drop (token) 'assign)
	       #(assign ,(cons n1 (p-delimited p-yield-or-testlist 'assign))))
	      ((match/save (token) 'augassign)
	       #(augassign ,saved-data ,n1 ,(p-yield-or-testlist)))
	      (else n1))))

    ;; small_stmt: (expr_stmt | print_stmt  | del_stmt | pass_stmt | flow_stmt |
    ;;             import_stmt | global_stmt | exec_stmt | assert_stmt)
    (define (p-small-stmt)
      (cond ((match/drop (token) 'del)
	     #(del ,(p-exprlist)))
	    ((match/drop (token) 'pass)
	     #(pass))
	    ((match/drop (token) 'break)
	     #(break))
	    ((match/drop (token) 'continue)
	     #(continue))
	    ((match/drop (token) 'return)
	     #(return ,(p-testlist)))
	    ((match/drop (token) 'yield)
	     #(yield ,(p-testlist)))
	    ((match/drop (token) 'raise)
	     ;; newer raise stmt only
	     #(raise ,(p-test)))
	    ;; XXX import, global, exec, assert...
	    (else
	     (p-expr-stmt))
	    ))
	    
    (define (p-simple-stmt)
      (let loop ((stmts (cons (p-small-stmt) '())))
	(cond ((match/drop (token) 'newline)
	       #(sequence ,(reverse stmts)))
	      ((match/drop (token) 'semicolon)
	       (loop (cons (p-small-stmt) stmts)))
	      (else
	       (syntax-error 'simple-stmt (token))))))
	     
    (define (p-stmt)
      ;; hack for now
      (p-simple-stmt))

    (define (p-stmt)
      (case (token.class (token))
	((if) (next) (p-if-stmt))
	((while) (next) (p-while-stmt))
	((for) (next) (p-for-stmt))
	((try) (next) (p-try-stmt))
	((def) (next) (p-funcdef))
	((class) (next) (p-classdef))
	(else (p-simple-stmt))))

    ;; stmt: simple_stmt | compound_stmt
    ;; simple_stmt: small_stmt (';' small_stmt)* [';'] NEWLINE
    ;; small_stmt: (expr_stmt | print_stmt  | del_stmt | pass_stmt | flow_stmt |
    ;;              import_stmt | global_stmt | exec_stmt | assert_stmt)
    
    ;; print_stmt: 'print' [testlist]
    ;; del_stmt: 'del' exprlist
    ;; pass_stmt: 'pass'
    ;; flow_stmt: break_stmt | continue_stmt | return_stmt | raise_stmt | yield_stmt
    ;; break_stmt: 'break'
    ;; continue_stmt: 'continue'
    ;; return_stmt: 'return' [testlist]
    ;; yield_stmt: yield_expr
    ;; raise_stmt: 'raise' [test [',' test [',' test]]]

    ;; compound_stmt: if_stmt | while_stmt | for_stmt | try_stmt | with_stmt | funcdef | classdef
    ;; if_stmt: 'if' test ':' suite ('elif' test ':' suite)* ['else' ':' suite]
    ;; while_stmt: 'while' test ':' suite ['else' ':' suite]
    ;; for_stmt: 'for' exprlist 'in' testlist ':' suite ['else' ':' suite]
    ;; try_stmt: ('try' ':' suite
    ;;            ((except_clause ':' suite)+
    ;;             ['else' ':' suite]
    ;;             ['finally' ':' suite] |
    ;;            'finally' ':' suite))
    ;; # NB compile.c makes sure that the default except clause is last
    ;; except_clause: 'except' [test [',' test]]

    ;; suite: simple_stmt | NEWLINE INDENT stmt+ DEDENT
    (define (p-suite)
      (if (match/drop (token) 'newline)
	  (if (match/drop (token) 'indent)
	      (let loop ((stmts (cons (p-stmt) '())))
		(if (match/drop (token) 'dedent)
		    #(suite ,(reverse stmts))
		    (loop (cons (p-stmt) stmts))))
	      (syntax-error 'indent (token)))
	  (p-simple-stmt)))

    ;; if_stmt: 'if' test ':' suite ('elif' test ':' suite)* ['else' ':' suite]
    (define (p-if-stmt)
      (let* ((i-test (p-test))
	     (i-body (expect 'colon p-suite #t))
	     (i-cases
	      (let loop ((cases '()))
		(cond ((match/drop (token) 'elif)
		       (let* ((test1 (p-test))
			      (body1 (expect 'colon p-suite #t)))
			 (loop (cons #(elif ,test1 ,body1) cases))))
		      ((match/drop (token) 'else)
		       (let* ((body1 (expect 'colon p-suite #t)))
			 (reverse (cons #(else ,body1) cases))))
		      (else
		       (reverse cases))))))
	#(if ,i-test ,i-body ,i-cases)))

    ;; while_stmt: 'while' test ':' suite ['else' ':' suite]
    (define (p-while-stmt)
      (let* ((w-test (p-test))
	     (w-body (expect 'colon p-suite #t))
	     (w-else (expect2 'else 'colon p-suite #f)))
	#(while ,w-test ,w-body ,w-else)))

    ;; for_stmt: 'for' exprlist 'in' testlist ':' suite ['else' ':' suite]
    (define (p-for-stmt)
      (let* ((for-clause (p-exprlist))
	     (in-clause (expect 'in p-testlist #t))
	     (body-clause (expect 'colon p-suite #t))
	     (else-clause (expect2 'else 'colon p-suite #f)))
	#(for ,for-clause ,in-clause ,body-clause ,else-clause)))

    ;; except_clause: 'except' [test [',' test]]
    (define (p-except-clause)
      (if (match (token) 'colon)
	  #(except #f #f)
	  (let ((n1 (p-test)))
	    (if (match/drop (token) 'comma)
		#(except ,n1 ,(p-test))
		#(except ,n1 #f)))))

    ;; try_stmt: ('try' ':' suite
    ;;            ((except_clause ':' suite)+
    ;;             ['else' ':' suite]
    ;;             ['finally' ':' suite] |
    ;;            'finally' ':' suite))

    (define (p-try-stmt)
      (let* ((try-clause (expect 'colon p-suite #t))
	     (except-clauses
	      (let loop ((l '()))
		(if (match/drop (token) 'except)
		    (loop (cons (p-except-clause) l))
		    l)))
	     (else-clause (expect2 'else 'colon p-suite #f))
	     (finally-clause (expect2 'finally 'colon p-suite #f)))
	#(try ,try-clause ,except-clauses ,else-clause ,finally-clause)))

    ;; with_stmt: 'with' test [ with_var ] ':' suite
    ;; with_var: ('as' | NAME) expr
    ;; XXX I'm not going to bother with with right now.

    ;; fpdef: NAME | '(' fplist ')'
    ;; fplist: fpdef (',' fpdef)* [',']

    (define (p-fpdef)
      (let* ((name (expect/data 'ident #f)))
	(if name
	    name
	    (expect 'lparen p-fplist #t))))

    (define (p-fplist)
      (let loop ((fpdefs (cons (p-fpdef) '())))
	(cond ((match/drop (token) 'comma)
	       (loop (cons (p-fpdef) fpdefs)))
	      ((match/drop (token) 'rparen)
	       fpdefs)
	      (else
	       (syntax-error 'rparen (token))))))


    ;; parameters: '(' [varargslist] ')'
    ;; varargslist: ((fpdef ['=' test] ',')*
    ;;               ('*' NAME [',' '**' NAME] | '**' NAME) |
    ;;               fpdef ['=' test] (',' fpdef ['=' test])* [','])

    (define (p-varargslist)
      (let loop ((formals '()))
	(cond ((match/drop (token) 'rparen)
	       (reverse formals))
	      ((match/drop (token) 'power)
	       (loop (cons #(keyargs ,(expect/data 'ident #t)) formals)))
	      ((match-string/drop (token) 'mulop "*")
	       (loop (cons #(restargs ,(expect/data 'ident #t)) formals)))
	      (else
	       (let ((fpdef (p-fpdef)))
		 (%printn fpdef)
		 (cond ((match/drop (token) 'comma)
			(loop (cons fpdef formals)))
		       ((match/drop (token) 'assign)
			(loop (cons #(default ,fpdef ,(p-test)) formals)))
		       ((match/drop (token) 'rparen)
			(reverse (cons fpdef formals)))
		       (else
			(syntax-error 'rparen (token)))
		       )
		 )))))

    ;; funcdef: [decorators] 'def' NAME parameters ':' suite
    ;; XXX no decorators for now
    (define (p-funcdef)
      (let* ((name (expect/data 'ident #t))
	     (params (expect 'lparen p-varargslist #t))
	     (body (expect 'colon p-suite #t)))
	#(function ,name ,params ,body)))

    ;; classdef: 'class' NAME ['(' [testlist] ')'] ':' suite
    (define (p-classdef)
      (let* ((name (expect/data 'ident #t))
	     (subclasses (expect 'lparen p-testlist #f))
	     (body (expect 'colon p-suite #t)))
	#(class ,name ,subclasses ,body)))

    ;; ================================================================================

    (next)
    (p-stmt)
    ))

(parse "tests/example.py")
