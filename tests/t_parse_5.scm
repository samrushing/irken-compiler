
(include "lib/core.scm")
(include "lib/frb.scm")
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

    (define (syntax-error expected)
      (print-string "syntax error, expected <")
      (%print expected)
      (print-string "> but got ")
      (%print current-token)
      (print-string "\n")
      (error "parse failed"))
    
    (define (token.class tok) (car tok))
    
    (define (token.data tok) (cdr tok))
    
    (define (get-indent tok)
      (cond ((eq? (token.class tok) 'whitespace)
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
		  (else tok)))))))

    (define (token) current-token)

    (define (next)
      (set! current-token (next-token))
      (set! start-of-line (eq? (token.class current-token) 'newline))
      )

    (define (match class)
      (eq? class (token.class current-token)))

    (define (match-string/drop class data)
      (cond ((and (match class)
		  (string-=? data (token.data current-token)))
	     (next)
	     #t)
	    (else #f)))

    (define (match/drop class)
      (cond ((match class)
	     (next)
	     #t)
	    (else #f)))

    (define (match/save class)
      (cond ((match class)
	     (set! saved-data (token.data current-token))
	     (next)
	     #t)
	    (else #f)))

    (define (expect class yes-proc error?)
      (if (match/drop class)
	  (yes-proc)
	  (if error?
	      (syntax-error class)
	      #f)))

    (define (expect2 c1 c2 yes-proc error?)
      (if (match/drop c1)
	  (expect c2 yes-proc #t)
	  (if error?
	      (syntax-error c1)
	      #f)))

    (define (expect/data class error?)
      (cond ((match class)
	     (let ((data (token.data current-token)))
	       (next)
	       data))
	    (error?
	     (syntax-error class))
	    (else #f)))

    ;; >>> taking inspiration from Pyrex's recursive-descent parser <<<

    ;; atom: ('(' [yield_expr|testlist_gexp] ')' | '[' [listmaker] ']' | '{' [dictmaker] '}' | '`' testlist1 '`' | NAME | NUMBER | STRING+)

    (define (p-atom)
      (let ((tok current-token))
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
	   (syntax-error 'atom)))))

    ;; listmaker: test ( list_for | (',' test)* [','] )
    ;; XXX not doing <list_for> for now
    (define (p-listmaker)
      (let loop ((list (cons (p-test) '())))
	(if (match/drop 'rbrace)
	    list
	    (if (match/drop 'comma)
		(loop (cons (p-test) list))
		(syntax-error 'comma)))))

    ;;dictmaker: test ':' test (',' test ':' test)* [',']
    (define (p-dictpair)
      (let ((n1 (p-test)))
	(if (match/drop 'colon)
	    (cons n1 (p-test))
	    (syntax-error 'colon))))

    (define (p-dictmaker)
      (let loop ((list (cons (p-dictpair) '())))
	(if (match/drop 'rbracket)
	    list
	    (if (match/drop 'comma)
		(loop (cons (p-dictpair) list))
		(syntax-error 'comma)))))

    ;; trailer: '(' [arglist] ')' | '[' subscriptlist ']' | '.' NAME
    (define (p-trailer atom)
      (let loop ((trailers (cons atom '())))
	(cond ((match/drop 'lparen)
	       (loop (cons #(call ,(p-arglist)) trailers)))
	      ((match/drop 'lbrace)
	       (loop (cons (p-subscript) trailers)))
	      ((match/drop 'getattr)
	       (if (match 'ident)
		   (let ((name (token.data current-token)))
		     (next)
		     (loop (cons #(getattr ,name) trailers)))
		   (syntax-error 'ident)))
	      (else trailers))))

    ;; power: atom trailer* ['**' factor]
    (define (p-power)
      (let* ((atom (p-atom)))
	(case (token.class current-token)
	  ((lparen lbrace getattr)
	   (p-trailer atom))
	  ((power)
	   (next)
	   #(binop power ,atom ,(p-factor)))
	  (else
	   atom))))

    ;; factor: ('+'|'-'|'~') factor | power
    (define (p-factor)
      (cond ((match-string/drop 'addop "+")
	     (p-factor))
	    ((match-string/drop 'addop "-")
	     #(unary-minus ,(p-factor)))
	    ((match/drop 'bitnot)
	     #(bitnot ,(p-factor)))
	    (else
	     (p-power))))

    ;; helper for binary operators
    (define (p-binop class p)
      (let loop ((n1 (p))
		 (tok current-token))
	(cond ((match/drop class)
	       (loop #(binop ,(token.data tok) ,n1 ,(p)) current-token))
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
	(cond ((match/drop 'is)
	       (cond ((match/drop 'not)
		      #(binop "is-not" ,n1 ,(p-compare)))
		     (else
		      #(binop "is" ,n1 ,(p-compare)))))
	      (else n1))))
    
    (define (p-in)
      (let ((n1 (p-is)))
	(cond ((match/drop 'in)
	       #(binop "in" ,n1 ,(p-is)))
	      ((match/drop 'not)
	       (cond ((match/drop 'in)
		      #(binop "not-in" ,n1 ,(p-is)))
		     (else
		      #(binop "in" ,n1 ,(p-is)))))
	      (else n1))))

    ;; not_test: 'not' not_test | comparison
    (define (p-not-test)
      (if (match/drop 'not)
	  #(not ,(p-not-test))
	  (p-in)))

    ;; and_test: not_test ('and' not_test)*
    (define (p-and-test)
      (let ((n1 (p-not-test)))
	(cond ((match/drop 'and)
	       #(and ,n1 ,(p-not-test)))
	      (else n1))))

    ;; or_test: and_test ('or' and_test)*
    (define (p-or-test)
      (let ((n1 (p-and-test)))
	(cond ((match/drop 'or)
	       #(or ,n1 ,(p-and-test)))
	      (else n1))))

    ;; test: or_test ['if' or_test 'else' test] | lambdef
    (define (p-test)
      (if (match/drop 'lambda)
	  (p-lambdef)
	  (let ((n1 (p-or-test)))
	    (if (match/drop 'if)
		(let ((n2 (p-or-test)))
		  (if (match/drop 'else)
		      #(test ,n1 ,n2 ,(p-test))
		      (syntax-error 'else)))
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
	  (%printn expr)
	  (cond ((match/drop 'comma)
		 (loop (cons expr exprs)))
		((eq? (token.class current-token) 'in)
		 ;; we're done
		 (cons expr exprs))
		(else
		 (syntax-error 'exprlist))))))

    ;; gen_iter: gen_for | gen_if
    ;; gen_for: 'for' exprlist 'in' or_test [gen_iter]
    ;; gen_if: 'if' old_test [gen_iter]

    ;; these three p-gen-xxx funs act more like probes than normal
    ;;   parsing functions - they will return #f rather than raising
    ;;   a syntax error...
    (define (p-gen-for)
      (if (match/drop 'for)
	  (let ((exprlist (p-exprlist)))
	    (if (match/drop 'in)
		(let* ((or-test (p-or-test))
		       (gen-iter-probe (p-gen-iter)))
		  (if gen-iter-probe
		      #(gen-for ,exprlist ,or-test ,gen-iter-probe)
		      #(gen-for ,exprlist ,or-test)))
		(syntax-error 'in)))
	  #f))

    (define (p-gen-if)
      (if (match/drop 'if)
	  (let ((old-test (p-old-test))
		(gen-iter-probe (p-gen-iter)))
	    (if gen-iter-probe
		#(gen-if ,old-test ,gen-iter-probe)
		#(gen-if ,old-test)))
	  #f))

    (define (p-gen-iter)
      (case (token.class current-token)
	((for) (p-gen-for))
	((if) (p-gen-if))
	(else #f)))

    ;; list_iter: list_for | list_if
    ;; list_for: 'for' exprlist 'in' testlist_safe [list_iter]
    ;; list_if: 'if' old_test [list_iter]

    ;; argument: test [gen_for] | test '=' test  # Really [keyword '='] test
    (define (p-argument)
      (let ((n1 (p-test)))
	(cond ((match/drop 'assign)
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
	(cond ((match/drop 'rparen)
	       args)
	      ((match-string/drop 'mulop "*")
	       (loop (cons #(restargs ,(p-argument)) args)))
	      ((match/drop 'power)
	       (loop (cons #(keyargs  ,(p-argument)) args)))
	      ((match/drop 'comma)
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
      (let ((n1 (if (match/drop 'colon) #f (p-test))))
	(let ((n2 (if (match 'rbrace) #f (p-test))))
	  #(slice ,n1 ,n2))))

    ;; ================================================================================

    ;; stmt: simple_stmt | compound_stmt
    ;; simple_stmt: small_stmt (';' small_stmt)* [';'] NEWLINE
    
    ;; how about we just 'inline' testlist here?
    ;; testlist: test (',' test)* [',']
    
    (define (p-testlist)
      (let loop ((list '()))
	(case (token.class current-token)
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
	(if (match/drop separator)
	    (loop (cons (p) result))
	    result)))

    (define (p-yield-or-testlist)
      (if (match/drop 'yield)
	  #(yield ,(p-testlist))
	  (p-testlist)))

    ;; expr_stmt: testlist (augassign (yield_expr|testlist) | ('=' (yield_expr|testlist))*)
    ;; augassign: ('+=' | '-=' | '*=' | '/=' | '%=' | '&=' | '|=' | '^=' | '<<=' | '>>=' | '**=' | '//=')
    (define (p-expr-stmt)
      (let ((n1 (p-testlist)))
	(cond ((match/drop 'assign)
	       #(assign ,(cons n1 (p-delimited p-yield-or-testlist 'assign))))
	      ((match/save 'augassign)
	       #(augassign ,saved-data ,n1 ,(p-yield-or-testlist)))
	      (else n1))))

    ;; small_stmt: (expr_stmt | print_stmt  | del_stmt | pass_stmt | flow_stmt |
    ;;             import_stmt | global_stmt | exec_stmt | assert_stmt)
    (define (p-small-stmt)
      (cond ((match/drop 'del)
	     #(del ,(p-exprlist)))
	    ((match/drop 'pass)
	     #(pass))
	    ((match/drop 'break)
	     #(break))
	    ((match/drop 'continue)
	     #(continue))
	    ((match/drop 'return)
	     #(return ,(p-testlist)))
	    ((match/drop 'yield)
	     #(yield ,(p-testlist)))
	    ((match/drop 'raise)
	     ;; newer raise stmt only
	     #(raise ,(p-test)))
	    ;; XXX import, global, exec, assert...
	    (else
	     (p-expr-stmt))
	    ))
	    
    ;; simple_stmt: small_stmt (';' small_stmt)* [';'] NEWLINE
    ;; small_stmt: (expr_stmt | print_stmt  | del_stmt | pass_stmt | flow_stmt |
    ;;              import_stmt | global_stmt | exec_stmt | assert_stmt)

    (define (p-simple-stmt)
      (let loop ((stmts (cons (p-small-stmt) '())))
	(cond ((match/drop 'newline)
	       #(sequence ,(reverse stmts)))
	      ((match/drop 'semicolon)
	       (loop (cons (p-small-stmt) stmts)))
	      (else
	       (syntax-error 'simple-stmt)))))
	     
    ;; stmt: simple_stmt | compound_stmt
    ;; compound_stmt: if_stmt | while_stmt | for_stmt | try_stmt | with_stmt | funcdef | classdef

    (define (p-stmt)
      (case (token.class current-token)
	((if) (next) (p-if-stmt))
	((while) (next) (p-while-stmt))
	((for) (next) (p-for-stmt))
	((try) (next) (p-try-stmt))
	((def) (next) (p-funcdef))
	((class) (next) (p-classdef))
	(else (p-simple-stmt))))

    ;; print_stmt: 'print' [testlist]
    ;; del_stmt: 'del' exprlist
    ;; pass_stmt: 'pass'
    ;; flow_stmt: break_stmt | continue_stmt | return_stmt | raise_stmt | yield_stmt
    ;; break_stmt: 'break'
    ;; continue_stmt: 'continue'
    ;; return_stmt: 'return' [testlist]
    ;; yield_stmt: yield_expr
    ;; raise_stmt: 'raise' [test [',' test [',' test]]]

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
      (if (match/drop 'newline)
	  (if (match/drop 'indent)
	      (let loop ((stmts (cons (p-stmt) '())))
		(if (match/drop 'dedent)
		    #(suite ,(reverse stmts))
		    (loop (cons (p-stmt) stmts))))
	      (syntax-error 'indent))
	  (p-simple-stmt)))

    ;; if_stmt: 'if' test ':' suite ('elif' test ':' suite)* ['else' ':' suite]
    (define (p-if-stmt)
      (let* ((i-test (p-test))
	     (i-body (expect 'colon p-suite #t))
	     (i-cases
	      (let loop ((cases '()))
		(cond ((match/drop 'elif)
		       (let* ((test1 (p-test))
			      (body1 (expect 'colon p-suite #t)))
			 (loop (cons #(elif ,test1 ,body1) cases))))
		      ((match/drop 'else)
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
      (if (match 'colon)
	  #(except #f #f)
	  (let ((n1 (p-test)))
	    (if (match/drop 'comma)
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
		(if (match/drop 'except)
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
	(cond ((match/drop 'comma)
	       (loop (cons (p-fpdef) fpdefs)))
	      ((match/drop 'rparen)
	       fpdefs)
	      (else
	       (syntax-error 'rparen)))))


    ;; parameters: '(' [varargslist] ')'
    ;; varargslist: ((fpdef ['=' test] ',')*
    ;;               ('*' NAME [',' '**' NAME] | '**' NAME) |
    ;;               fpdef ['=' test] (',' fpdef ['=' test])* [','])

    (define (p-varargslist)
      (let loop ((formals '()))
	(cond ((match/drop 'rparen)
	       (reverse formals))
	      ((match/drop 'power)
	       (loop (cons #(keyargs ,(expect/data 'ident #t)) formals)))
	      ((match-string/drop 'mulop "*")
	       (loop (cons #(restargs ,(expect/data 'ident #t)) formals)))
	      (else
	       (let ((fpdef (p-fpdef)))
		 (%printn fpdef)
		 (cond ((match/drop 'comma)
			(loop (cons fpdef formals)))
		       ((match/drop 'assign)
			(loop (cons #(default ,fpdef ,(p-test)) formals)))
		       ((match/drop 'rparen)
			(reverse (cons fpdef formals)))
		       (else
			(syntax-error 'rparen))
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
