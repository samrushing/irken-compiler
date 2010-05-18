(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/alist.scm")
(include "lib/string.scm")
(include "lib/frb.scm")
(include "lib/symbol.scm")
(include "lib/io.scm")

(include "parse/lexstep.scm")
(include "lib/lexer.scm")

;; parser tables

(datatype action
  (:shift int)
  (:reduce int int))

(datatype action-list
  (:nil)
  (:cons int (action) (action-list)))

(datatype goto-list
  (:nil)
  (:cons int int (goto-list)))

(include "parse/t2.scm")

(datatype item
  (:nt symbol (list (item)))
  (:t symbol string)
  )

(datatype stack
  (:empty)
  (:elem (item) int (stack))
  )

(define (parse path)
  (let ((file (file:open-read path))
	(token-gen (make-lex-generator file))
	(paren-stack (list:nil))
	(indents (list:cons 0 (list:nil)))
	(start-of-line #t)
	(held-token eof-token)
	(tok eof-token)
	)
    
    (define get-indent
      ;; XXX handle or disallow tabs
      (token:t 'whitespace str) -> (string-length str)
      ;; non-whitespace at the front of a line
      (token:t _ _)             -> 0)

    (define (get-top-indent)
      (match indents with
        () -> 0
	(indent . _) -> indent))

    (define (next-token0)
      ;; process (i.e., filter/synthesize) the token stream
      (let loop ()
	(cond ((not (eq? held-token eof-token))
	       (set! tok held-token)
	       (set! held-token eof-token))
	      (else
	       (set! tok (token-gen))
	       ;;(print "token-gen: ") (printn tok)
	       ))
	;;(print "next-token loop ") (printn start-of-line)
	(if start-of-line
	    ;; in this state we might emit INDENT/DEDENT
	    (let ((this-indent (get-indent tok))
		  (top-indent (get-top-indent)))
	      (set! start-of-line #f)
	      (set! held-token tok)
	      (cond ((> this-indent top-indent)
		     (set! indents (list:cons this-indent indents))
		     (token:t 'INDENT ""))
		    ((< this-indent top-indent)
		     (set! indents (cdr indents))
		     ;; go around again, might be more DEDENT
		     (set! start-of-line #t)
		     (token:t 'DEDENT ""))
		    (else
		     (loop))))
	    ;; in the middle of a line somewhere
	    (match tok with
	      (token:t 'NEWLINE _)
	      -> (match paren-stack with
		   () -> (begin (set! start-of-line #t) tok)
		   _  -> (loop))
	      (token:t 'whitespace _) -> (loop)
	      (token:t 'comment _ )   -> (loop)
	      (token:t _ _) -> tok
	      ))
	))

    (define (next-token)
      (let ((t (next-token0)))
	(print-string "next-token: ") (printn t)
	t))

    (let ((stack (stack:empty)))

      (define (get-state)
	(match stack with
	  (stack:empty)          -> 0
	  (stack:elem _ state _) -> state
	  ))

      (define (lookup-action state kind)
	(let loop ((l actions[state]))
	  (match l with
            (action-list:nil)
	    -> (error "missing action?")
	    (action-list:cons tkind action tl)
	    -> (if (eq? terminals[tkind] kind)
		   action
		   (loop tl)))))

      (define (lookup-goto state nt)
	(let loop ((l goto[state]))
	  (match l with
	     (goto-list:nil)
	     -> (error "missing goto?")
	     (goto-list:cons nt0 new-state tl)
	     -> (if (eq? nt0 nt)
		    new-state
		    (loop tl)))))

      (define (pop-n n)
	(let loop ((n n) (result (list:nil)))
	  (if (= n 0)
	      result
	      (loop (- n 1) (list:cons (pop) result)))))

      (define (push item state)
	(set! stack (stack:elem item state stack)))

      (define (pop)
	(match stack with
	   (stack:elem item _ rest) -> (begin (set! stack rest) item)
	   (stack:empty) -> (error "stack underflow")))
	   
      (let loop ((tok (next-token)))
	(cond ((eq? tok eof-token) (pop) (pop))
	      (else
	       ;;(print-string "token: ") (printn tok)
	       ;;(print-string "state: ") (printn (get-state))
	       ;;(print "indentation: ") (printn indentation)
	       (vcase token tok
		 ((:t kind val)
		  (let ((a (lookup-action (get-state) kind)))
		    (vcase action a
		      ((:shift state)
		       (push (item:t kind val) state)
		       (loop (next-token)))
		      ((:reduce plen nt)
		       (let ((args (pop-n plen))
			     (next-state (lookup-goto (get-state) nt)))
			 (push (item:nt non-terminals[nt] args) next-state))
		       (loop tok)))))))))
      )))

(define (indent n)
  (let loop ((n n))
    (cond ((= n 0) #t)
	  (else
	   (print-string "  ")
	   (loop (- n 1))))))

(define (print-parse-tree t)

  (let loop0 ((d 0)
	      (t t))
    (indent d)
    (vcase item t
      ((:t sym str)
       (print sym) (print-string " ") (printn str))
      ((:nt sym items)
       (printn sym)
       (let loop1 ((l items))
	 (vcase list l
	    ((:nil) #u)
	    ((:cons item tail)
	     (loop0 (+ d 1) item)
	     (loop1 tail)))))))
  )
      
;; print a parse tree out in a way that facilitates writing patterns for it.
(define ppt
  (item:nt sym items) -> (begin (print-string "(item:nt ") (print sym) (print-string " ") (ppt-list items) (print-string ")"))
  (item:t  sym str)   -> (begin (print-string "(item:t ") (print sym) (print-string " \"") (print-string str) (print-string "\")"))
  )

(define (ppt-list l)
  (print-string "(")
  (ppt-list2 l))

(define ppt-list2
  () -> (print-string ")")
  (hd . tl) -> (begin (ppt hd) (print-string " ") (ppt-list2 tl))
  )

(datatype formal
  (:var string)
  ;;(:var-with-default string (expr))
  )

(datatype ifclause
  (:case (expr) (expr))
  )

(datatype expr
  (:int int)
  (:string string)
  (:varref string)
  (:binary string (expr) (expr))
  (:unary string (expr))
  (:funcall (expr) (list (expr)))
  (:getitem (expr) (expr))
  (:getattr (expr) string)
  (:lambda (list (formal)) (expr))
  (:sequence (list (expr)))
  (:function string (list (formal)) (expr))
  (:if (list (ifclause)) (expr))
  (:while (expr) (expr) (expr))
  (:for (expr) (expr) (expr) (expr))
  (:break)
  (:continue)
  (:pass)
  (:raise (expr))
  (:return (expr))
  (:unparsed symbol (list (expr)))
  )

(define (ppt-expr d e)
  (print-string "\n")
  (indent d)
  (match e with
    (expr:int n) -> (begin (print n) #u)
    (expr:string s) -> (begin (print s) #u)
    (expr:varref s) -> (begin (print-string "varref ") (print-string s) #u)
    (expr:binary op a b) -> (begin (print-string "binary ") (print-string op) (ppt-expr (+ d 1) a) (ppt-expr (+ d 1) b) #u)
    (expr:unary op a) -> (begin (print-string "unary ") (print-string op) (ppt-expr (+ d 1) a) #u)
    (expr:funcall fun args) -> (begin (print-string "funcall ") (ppt-expr (+ d 1) fun) (ppt-expr-list (+ d 1) args) #u)
    (expr:getitem item index) -> (begin (print-string "getitem ") (ppt-expr (+ d 1) item) (ppt-expr (+ d 1) index) #u)
    (expr:getattr item attr) -> (begin (print-string "getattr ") (ppt-expr (+ d 1) item) (print-string " ") (print-string attr) #u)
    (expr:lambda formals body) -> (begin (print-string "lambda ") (print formals) (ppt-expr (+ d 1) body))
    (expr:sequence items) -> (begin (print-string "sequence ") (ppt-expr-list (+ d 1) items) #u)
    (expr:function name formals body) -> (begin (print-string "function ") (print-string name) (print-string " ") (print formals) (ppt-expr (+ d 1) body))
    (expr:if clauses else) -> (begin (print-string "if") (ppt-ifclause (+ d 1) clauses) (ppt-expr (+ d 1) else))
    (expr:while test body else) -> (begin (print-string "while") (ppt-expr (+ d 1) test) (ppt-expr (+ d 1) body) (ppt-expr (+ d 1) else))
    (expr:for vars src body else) -> (begin (print-string "for") (ppt-expr (+ d 1) vars) (ppt-expr (+ d 1) src) (ppt-expr (+ d 1) body) (ppt-expr (+ d 1) else))
    (expr:pass) -> (begin (print-string "pass") #u)
    (expr:break) -> (begin (print-string "break") #u)
    (expr:continue) -> (begin (print-string "continue") #u)
    (expr:raise exp) -> (begin (print-string "raise ") (ppt-expr (+ d 1) exp))
    (expr:return val) -> (begin (print-string "return") (ppt-expr (+ d 1) val))
    (expr:unparsed symbol args) -> (begin (print-string "unparsed ") (print symbol) (ppt-expr-list (+ d 1) args) #u)
    ))

(define (ppt-expr-list d l)
  (match l with
    () -> #u
    (hd . tl) -> (begin (ppt-expr d hd) (ppt-expr-list d tl))))

(define (ppt-ifclause d l)
  (match l with
    () -> #u
    ((ifclause:case test result) . tl) -> (begin (ppt-expr d test) (print-string "?") (ppt-expr (+ d 1) result) (ppt-ifclause d tl))))

(define (perror where x)
  (print-string "decode error in ")
  (print-string where)
  (print-string ": ")
  (printn x)
  (error "decode error"))

(define p-operator
  (item:nt _ ((item:t kind data))) -> data
  (item:t kind data) -> data
  x -> (perror "p-operator" x))

(define p-binary-splat
  e () -> e
  e (op arg (item:nt _ splat)) -> (expr:binary (p-operator op) e (p-binary-splat (p-expr arg) splat))
  e x -> (perror "p-binary-splat" x)
  )

(define p-binary
  (a (item:nt _ splat)) -> (p-binary-splat (p-expr a) splat)
  x -> (perror "p-binary" x))

(define p-power
  (arg0 trailer (item:nt _ bin-splat)) -> (p-binary-splat (p-trailer-splat (p-expr arg0) trailer) bin-splat)
  x -> (perror "p-power" x))

(define p-factor
  (unary f) -> (expr:unary (p-operator unary) (p-expr f))
  (power)   -> (p-expr power)
  x -> (perror "p-factor" x))

(define p-trailer-splat
  exp0 (item:nt _ ())    -> exp0
  exp0 (item:nt _ (trailer splat)) -> (p-trailer-splat (p-trailer exp0 trailer) splat)
  exp0 x -> (perror "p-trailer-splat" x)
  )

(define p-trailer
  exp0 (item:nt _ ((item:t 'lparen _) arglist _))        -> (expr:funcall exp0 (p-arglist arglist))
  exp0 (item:nt _ ((item:t 'lbracket _) exp1 _))         -> (expr:getitem exp0 (p-expr exp1))
  exp0 (item:nt _ ((item:t 'dot _) (item:t 'NAME name))) -> (expr:getattr exp0 name)
  exp0 x -> (perror "p-trailer" x)
  )

(define p-arglist
  (item:nt _ ()) -> (list:nil)
  _ -> (error "arglist"))

(define (p-formals formals)
  (define p-formals0
    () -> (list:nil)
    (_ (item:t _ name) (item:nt _ splat)) -> (list:cons (formal:var name) (p-formals0 splat))
    x -> (perror "p-formals0" x))
  (match formals with
    (item:nt _ ((item:t _ name0) (item:nt _ splat) _)) -> (list:cons (formal:var name0) (p-formals0 splat))
    x -> (perror "p-formals" x)))

(define p-funcdef
  ;; 'def' NAME '(' <formals> ')' ':' <suite>
  (_ (item:t _ name) _ (item:nt _ (formals)) _ _ (item:nt _ body))
  -> (expr:function name (p-formals formals) (p-suite body))
  x -> (perror "p-funcdef" x))

(define p-lambda
  (_ (item:nt _ (formals)) _ body) -> (expr:lambda (p-formals formals) (p-expr body))
  x -> (perror "p-lambda" x))

(define sequence
    () -> (expr:sequence (list:nil))
    (a) -> a
    l -> (expr:sequence l))

(define p-sequence
  acc () -> (sequence (reverse acc))
  acc (_ item (item:nt _ splat)) -> (p-sequence (list:cons (p-expr item) acc) splat)
  acc x -> (perror "p-sequence" x))

(define p-testlist
  (test0 (item:nt _ splat) _) -> (p-sequence (list:cons (p-expr test0) (list:nil)) splat)
  x -> (perror "p-testlist" x))

(define p-simple-stmt
  (small (item:nt _ splat) _ _) -> (p-sequence (list:cons (p-expr small) (list:nil)) splat)
  x -> (perror "p-simple-stmt" x))

(define (p-file-input l)
  (let loop ((acc (list:nil))
	     (l l))
    (match l with
      () -> (sequence acc)
      ((item:nt _ ((item:t 'NEWLINE _))) (item:nt _ splat)) -> (loop acc splat) ;; ignore NEWLINE tokens
      ((item:nt _ (item0)) (item:nt _ splat)) -> (loop (list:cons (p-expr item0) acc) splat)
      x -> (perror "p-file-input" x))
    ))

(define p-stmt+
  (exp0) -> (list:cons (p-expr exp0) (list:nil))
  (exp0 (item:nt _ plus)) -> (list:cons (p-expr exp0) (p-stmt+ plus))
  x -> (perror "p-stmt+" x))

(define p-suite
  ;; suite: simple_stmt | NEWLINE INDENT stmt+ DEDENT
  (stmt) -> (p-expr stmt)
  (_ _ (item:nt _ stmts) _) -> (sequence (p-stmt+ stmts))
  x -> (perror "p-suite" x))

(define p-return
  ;; return_stmt: 'return' [testlist]
  (_ (item:nt _ ())) -> (expr:return (expr:varref "None"))
  (_ (item:nt _ ((item:nt _ val)))) -> (expr:return (p-testlist val))
  x -> (perror "p-return" x))

(define p-raise
  ;; return_stmt: 'raise' [testlist]
  (_ (item:nt _ ())) -> (expr:raise (expr:pass))
  (_ (item:nt _ ((item:nt _ val)))) -> (expr:raise (p-testlist val))
  x -> (perror "p-raise" x))

(define p-elif-splat
  () -> (list:nil)
  ;; ('elif' test ':' suite)*
  (_ test _ (item:nt _ body) (item:nt _ splat)) -> (list:cons (ifclause:case (p-expr test) (p-suite body)) (p-elif-splat splat))
  x -> (perror "p-elif-splat" x))

(define p-else
  () -> (expr:pass)
  (_ _ (item:nt _ body)) -> (p-suite body)
  x -> (perror "p-else" x))

(define p-if-stmt
  ;; if_stmt: 'if' test ':' suite ('elif' test ':' suite)* ['else' ':' suite]
  (_ test _ (item:nt _ body) (item:nt _ splat) (item:nt _ else))
  -> (expr:if
      (list:cons
       (ifclause:case (p-expr test) (p-suite body))
       (p-elif-splat splat))
      (p-else else))
  x -> (perror "p-if-stmt" x))

(define p-while-stmt
  ;; while_stmt: 'while' test ':' suite ['else' ':' suite]
  (_ test _ (item:nt _ body) (item:nt _ else)) -> (expr:while (p-expr test) (p-suite body) (p-else else))
  x -> (perror "p-while-stmt" x))

(define p-for-stmt
  ;; for_stmt: 'for' exprlist 'in' testlist ':' suite ['else' ':' suite]
  (_ (item:nt _ vars) _ (item:nt _ src) _ (item:nt _ body) (item:nt _ else))
  -> (expr:for (p-testlist vars) (p-testlist src) (p-suite body) (p-else else))
  x -> (perror "p-for-stmt" x)
  )

(define p-list
  ()      -> (list:nil)
  (x . y) -> (list:cons (p-expr x) (p-list y))
  )

(define p-not-test
  (a) -> (p-expr a)
  (not a) -> (expr:unary "not" (p-expr a))
  x -> (perror "p-not-test" x))

(define p-one
  (a) -> (p-expr a)
  x -> (perror "p-one" x))

(define (strip-quotes s)
  (substring s 1 (- (string-length s) 1)))

(define p-string+
  (item:nt _ ((item:t _ s)))       -> (list:cons (strip-quotes s) (list:nil))
  (item:nt _ ((item:t _ s) splat)) -> (list:cons (strip-quotes s) (p-string+ splat))
  x -> (perror "p-string+" x))

(define p-atom
  ((item:t 'NUMBER val)) -> (expr:int (string->int val))
  ((item:t 'NAME val))   -> (expr:varref val)
  (string+) -> (expr:string (string-append (p-string+ string+)))
  x -> (perror "p-atom" x))

(define p-simple
  ((item:t 'break _))    -> (expr:break)
  ((item:t 'pass _))     -> (expr:pass)
  ((item:t 'continue _)) -> (expr:continue)
  x -> (perror "p-simple" x))

(define p-expr
  (let ((l (alist/new)))
    ;; store the parsing functions in an alist keyed by production rule.
    (define (A key val)
      (set! l (alist/add l key val)))
    (A 'expr           p-binary)
    (A 'xor_expr       p-binary)
    (A 'and_expr       p-binary)
    (A 'shift_expr     p-binary)
    (A 'arith_expr     p-binary)
    (A 'term           p-binary)
    (A 'comparison     p-binary)
    (A 'or_test        p-binary)
    (A 'and_test       p-binary)
    (A 'factor         p-factor)
    (A 'power          p-power)
    (A 'test           p-one)
    (A 'not_test       p-not-test)
    (A 'lambdef        p-lambda)
    (A 'testlist       p-testlist)
    (A 'exprlist       p-testlist)
    (A 'expr_stmt      p-binary)
    (A 'small_stmt     p-one)
    (A 'simple_stmt    p-simple-stmt)
    (A 'stmt           p-one)
    (A 'file_input     p-file-input)
    (A 'compound_stmt  p-one)
    (A 'funcdef        p-funcdef)
    (A 'suite          p-suite)
    (A 'flow_stmt      p-one)
    (A 'if_stmt        p-if-stmt)
    (A 'while_stmt     p-while-stmt)
    (A 'for_stmt       p-for-stmt)
    (A 'break_stmt     p-simple)
    (A 'continue_stmt  p-simple)
    (A 'pass_stmt      p-simple)
    (A 'raise_stmt     p-raise)
    (A 'return_stmt    p-return)
    (A 'atom           p-atom)
    (lambda (x)
      (match x with
        (item:t _ _)  -> (perror "p-expr" x)
	(item:nt kind val) -> (let ((probe (alist/lookup l kind)))
				(match probe with
				  (maybe:no) -> (expr:unparsed kind (p-list val))
				  (maybe:yes fun) -> (fun val)))
	))))

(let ((t (if (> (sys:argc) 1) (parse sys:argv[1]) (parse "tests/parse_2.py"))))
  (printn t)
  (print-parse-tree t)
  (ppt t)
  (terpri)
  (let ((exp (p-expr t)))
    (ppt-expr 0 exp)
    (print-string "\n")
    exp
    )
  )

