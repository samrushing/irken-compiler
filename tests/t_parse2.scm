;;; -*- Mode: Irken -*-

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

(include "parse/t2.scm")

(datatype item
  (:t  symbol (range) string)
  (:nt symbol (range) (list (item)))
  )

(datatype stack
  (:empty)
  (:elem (item) int (stack))
  )

;; this isn't very modular. yet. I'd like to get a generator-based parse going on here.
;;   might even obviate the need for tracking position in the AST. [since lexer position
;;   can propagate to the current parse error].
(define (parse path)
  (let ((file (file/open-read path))
	(token-gen (make-lex-generator file))
	(paren-stack (list:nil))
	(indents (list:cons 0 (list:nil)))
	(start-of-line #t)
	(held-token eof-token)
	(tok eof-token)
	)
    
    (define get-indent
      ;; XXX handle or disallow tabs
      (token:t 'whitespace str _) -> (string-length str)
      ;; non-whitespace at the front of a line
      (token:t _ _ _)             -> 0)

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
	    (match tok with
	      (token:t sym val range)
	      -> (let ((this-indent (get-indent tok))
		       (top-indent (get-top-indent)))
		   (set! start-of-line #f)
		   (set! held-token tok)
		   (cond ((> this-indent top-indent)
			  (set! indents (list:cons this-indent indents))
			  (token:t 'INDENT "" range))
			 ((< this-indent top-indent)
			  (set! indents (cdr indents))
			  ;; go around again, might be more DEDENT
			  (set! start-of-line #t)
			  (token:t 'DEDENT "" range))
			 (else
			  (loop)))))
	    ;; in the middle of a line somewhere
	    (match tok with
	      (token:t 'NEWLINE _ _)
	      -> (match paren-stack with
		   () -> (begin (set! start-of-line #t) tok)
		   _  -> (loop))
	      (token:t 'whitespace _ _) -> (loop)
	      (token:t 'comment _ _)   -> (loop)
	      (token:t _ _ _) -> tok
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

      (define (get-range args)
	(let loop ((args args) (l0 -1) (p0 -1) (l1 -1) (p1 -1))
	  (define test-range
	    -1 tl (range:t l2 p2 l3 p3) -> (loop tl l2 p2 l3 p3)
	     _ tl (range:t l2 p2 l3 p3) -> (loop tl l0 p0 l3 p3)
	     _ tl (range:f)             -> (loop tl l0 p0 l1 p1)
	     )
	  (match l0 args with
	     -1 ()                     -> (range:f)
	      _ ()                     -> (range:t l0 p0 l1 p1)
	      _ ((item:t  _ r _) . tl) -> (test-range l0 tl r)
	      _ ((item:nt _ r _) . tl) -> (test-range l0 tl r)
	      )))

      (let loop ((tok (next-token)))
	(cond ((eq? tok eof-token) (pop) (pop))
	      (else
	       (print-string "token: ") (printn tok)
	       ;;(print-string "state: ") (printn (get-state))
	       ;;(print "indentation: ") (printn indentation)
	       (vcase token tok
		 ((:t kind val range)
		  (let ((a (lookup-action (get-state) kind)))
		    (vcase action a
		      ((:shift state)
		       (push (item:t kind range val) state)
		       (loop (next-token)))
		      ((:reduce plen nt)
		       (let ((args (pop-n plen))
			     (next-state (lookup-goto (get-state) nt)))
			 (push (item:nt non-terminals[nt] (get-range args) args) next-state))
		       (loop tok)))
		    )))
	       )))
      )))

(define indent
  0 -> #t
  n -> (begin (print-string "  ") (indent (- n 1))))

(define (print-parse-tree t)
  (let loop0 ((d 0)
	      (t t))
    (indent d)
    (match t with
      (item:t sym range str)
      -> (begin (print range) (print-string " ") (print sym) (print-string " ") (printn str))
      (item:nt sym range items)
      -> (begin
	   (print range)
	   (print-string " ")
	   (printn sym)
	   (let loop1 ((l items))
	     (match l with
	       () -> #u
	       (hd . tl) -> (begin (loop0 (+ d 1) hd) (loop1 tl)))))
      )))

;; print a parse tree out in a way that facilitates writing patterns for it.
(define ppt
  (item:nt sym range items) -> (begin (print-string "(item:nt ") (print sym) (print-string " ") (ppt-list items) (print-string ")"))
  (item:t  sym range str)   -> (begin (print-string "(item:t ") (print sym) (print-string " \"") (print-string str) (print-string "\")"))
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

(datatype literal
  (:int int)
  (:string string)
  )

(datatype params
  (:literal (literal))
  (:varref string)
  (:function string (list (formal)))	;; <body>
  (:unparsed symbol)
  (:for (list (formal)))
  (:none)
  )

(define (perror where x)
  (print-string "decode error in ")
  (print-string where)
  (print-string ": ")
  (printn x)
  (error "decode error"))

(define p-operator
  (item:nt _ _ ((item:t kind _ data))) -> data
  (item:t _ _ data) -> data
  x -> (perror "p-operator" x))

(define NR (range:f))

(define (make-varref name) {t='varref p=(params:varref name) subs='() range=NR})

(define p-binary-splat
  e () -> e
  e (op arg (item:nt _ _ splat))
  -> {t='call p=(params:none) subs=(LIST (make-varref (p-operator op)) e (p-binary-splat (p-expr arg) splat)) range=NR}
  e x -> (perror "p-binary-splat" x)
  )

(define p-binary
  (e (item:nt _ _ splat)) -> (p-binary-splat (p-expr e) splat)
  x -> (perror "p-binary" x))

(define p-power
  (arg0 trailer (item:nt _ _ splat)) -> (p-binary-splat (p-trailer-splat (p-expr arg0) trailer) splat)
  x -> (perror "p-power" x))

(define p-factor
   (unary f) -> {t='call p=(params:none) subs=(LIST (make-varref (p-operator unary)) (p-expr f)) range=NR}
   (power)   -> (p-expr power)
   x -> (perror "p-factor" x))

(define p-trailer-splat
  exp0 (item:nt _ _ ())    -> exp0
  exp0 (item:nt _ _ (trailer splat)) -> (p-trailer-splat (p-trailer exp0 trailer) splat)
  exp0 x -> (perror "p-trailer-splat" x)
  )

(define __getitem__ {t='varref p=(params:varref "__getitem__") subs='() range=NR})
(define __getattr__ {t='varref p=(params:varref "__getattr__") subs='() range=NR})

(define pass-node   {t='pass p=(params:none) subs='() range=NR})

(define (literal-string s r) {t='literal p=(params:literal (literal:string s)) subs='() range=r})

(define p-trailer
  exp0 (item:nt _ _ ((item:t 'lparen _ _) arglist _))           -> {t='call p=(params:none) subs=(list:cons exp0 (p-arglist arglist)) range=NR}
  exp0 (item:nt _ _ ((item:t 'lbracket _ _) exp1 _))            -> {t='call p=(params:none) subs=(LIST __getitem__ exp0 (p-expr exp1)) range=NR}
  exp0 (item:nt _ _ ((item:t 'dot _ _) (item:t 'NAME nr name))) -> {t='call p=(params:none) subs=(LIST __getattr__ exp0 (literal-string name nr)) range=NR}
  exp0 x -> (perror "p-trailer" x)
  )

(define p-arglist
  (item:nt _ _ ()) -> (list:nil)
  _ -> (error "arglist"))

(define (p-formals formals)
  (define p-formals0
    () -> (list:nil)
    (_ (item:t _ _ name) (item:nt _ _ splat)) -> (list:cons (formal:var name) (p-formals0 splat))
    x -> (perror "p-formals0" x))
  (match formals with
    (item:nt _ _ ((item:t _ _ name0) (item:nt _ _ splat) _)) -> (list:cons (formal:var name0) (p-formals0 splat))
    x -> (perror "p-formals" x)))

(define p-funcdef
  ;; 'def' NAME '(' <formals> ')' ':' <suite>
  (_ (item:t _ _ name) _ (item:nt _ _ (formals)) _ _ (item:nt _ _ body))
  -> {t='function p=(params:function name (p-formals formals)) subs=(LIST (p-suite body)) range=NR}
  x -> (perror "p-funcdef" x))

(define p-lambda
  (_ (item:nt _ _ (formals)) _ body) -> {t='function p=(params:function "lambda" (p-formals formals)) subs=(LIST (p-expr body)) range=NR}
  x -> (perror "p-lambda" x))

(define sequence
  ()  -> {t='sequence p=(params:none) subs='() range=NR}
  (a) -> a
  l   -> {t='sequence p=(params:none) subs=l range=NR}
  )

(define p-sequence
  acc () -> (sequence (reverse acc))
  acc (_ item (item:nt _ _ splat)) -> (p-sequence (list:cons (p-expr item) acc) splat)
  acc x -> (perror "p-sequence" x))

(define p-testlist
  (test0 (item:nt _ _ splat) _) -> (p-sequence (LIST (p-expr test0)) splat)
  x -> (perror "p-testlist" x))

(define p-simple-stmt
  (small (item:nt _ _ splat) _ _) -> (p-sequence (LIST (p-expr small)) splat)
  x -> (perror "p-simple-stmt" x))

(define (p-file-input l)
  (let loop ((acc (list:nil))
	     (l l))
    (match l with
      ()                                                          -> (sequence acc)
      ((item:nt _ _ ((item:t 'NEWLINE _ _))) (item:nt _ _ splat)) -> (loop acc splat) ;; ignore NEWLINE tokens
      ((item:nt _ _ (item0)) (item:nt _ _ splat))                 -> (loop (list:cons (p-expr item0) acc) splat)
      x -> (perror "p-file-input" x))
    ))

(define p-stmt+
  (exp0) -> (LIST (p-expr exp0))
  (exp0 (item:nt _ _ plus)) -> (list:cons (p-expr exp0) (p-stmt+ plus))
  x -> (perror "p-stmt+" x))

(define p-suite
  ;; suite: simple_stmt | NEWLINE INDENT stmt+ DEDENT
  (stmt) -> (p-expr stmt)
  (_ _ (item:nt _ _ stmts) _) -> (sequence (p-stmt+ stmts))
  x -> (perror "p-suite" x))

(define p-return
  ;; return_stmt: 'return' [testlist]
  (_ (item:nt _ _ ()))                  -> {t='return p=(params:none) subs='() range=NR}
  (_ (item:nt _ _ ((item:nt _ _ val)))) -> {t='return p=(params:none) subs=(LIST (p-testlist val)) range=NR}
  x -> (perror "p-return" x))

(define p-raise
  ;; return_stmt: 'raise' [testlist]
  (_ (item:nt _ _ ())) -> {t='raise p=(params:none) subs=(LIST pass-node) range=NR}
  (_ (item:nt _ _ ((item:nt _ _ val)))) -> {t='raise p=(params:none) subs=(LIST (p-testlist val)) range=NR}
  x -> (perror "p-raise" x))

(define p-elif-splat
  () -> '()
  ;; ('elif' test ':' suite)*
  (_ test _ (item:nt _ _ body) (item:nt _ _ splat)) -> (append (LIST (p-expr test) (p-suite body)) (p-elif-splat splat))
  x -> (perror "p-elif-splat" x))

(define p-else
  () -> pass-node
  (_ _ (item:nt _ _ body)) -> (p-suite body)
  x -> (perror "p-else" x))

(define p-if-stmt
  ;; if_stmt: 'if' test ':' suite ('elif' test ':' suite)* ['else' ':' suite]
  (_ test _ (item:nt _ _ body) (item:nt _ _ splat) (item:nt _ _ else))
  ;; urgh, this is a mess.  should try to turn it into a ternary-if, or a cond, or something.
  ;; probably the cleanest way is to pass <else> down to p-elif-splat
  -> {t='if p=(params:none) subs=(append (LIST (p-expr test) (p-suite body)) (append (p-elif-splat splat) (LIST (p-else else)))) range=NR}
  x -> (perror "p-if-stmt" x))

(define p-while-stmt
  ;; while_stmt: 'while' test ':' suite ['else' ':' suite]
  (_ test _ (item:nt _ _ body) (item:nt _ _ else)) -> {t='while p=(params:none) subs=(LIST (p-expr test) (p-suite body) (p-else else)) range=NR}
  x -> (perror "p-while-stmt" x))

(define p-for-stmt
  ;; for_stmt: 'for' exprlist 'in' testlist ':' suite ['else' ':' suite]
  (_ (item:nt _ _ vars) _ (item:nt _ _ src) _ (item:nt _ _ body) (item:nt _ _ else))
    -> {t='for p=(params:none) subs=(LIST (p-testlist vars) (p-testlist src) (p-suite body) (p-else else)) range=NR}
  x -> (perror "p-for-stmt" x)
  )

(define p-list
  ()      -> (list:nil)
  (x . y) -> (list:cons (p-expr x) (p-list y))
  )

(define p-not-test
  (a) -> (p-expr a)
  (not a) -> {t='call p=(params:none) subs=(LIST (make-varref "not") (p-expr a)) range=NR}
  x -> (perror "p-not-test" x)
  )

(define p-one
  (a) -> (p-expr a)
  x -> (perror "p-one" x))

(define p-simple
  ((item:t 'break _ _))    -> {t='break    p=(params:none) subs='() range=NR}
  ((item:t 'pass _ _))     -> {t='pass     p=(params:none) subs='() range=NR}
  ((item:t 'continue _ _)) -> {t='continue p=(params:none) subs='() range=NR}
  x -> (perror "p-simple" x))

(define (strip-quotes s)
  (substring s 1 (- (string-length s) 1)))

(define p-string+
  (item:nt _ _ ((item:t _ _ s)))       -> (LIST (strip-quotes s))
  (item:nt _ _ ((item:t _ _ s) splat)) -> (list:cons (strip-quotes s) (p-string+ splat))
  x -> (perror "p-string+" x))

(define p-atom
  ((item:t 'NUMBER r val)) -> {t='literal subs='() p=(params:literal (literal:int (string->int val))) range=r }
  ((item:t 'NAME r val))   -> {t='varref  subs='() p=(params:varref val) range=r }
  (string+)                -> {t='literal subs='() p=(params:literal (literal:string (string-append (p-string+ string+)))) range=NR }
  x -> (perror "p-atom" x))

(define parse-table
  (alist/make
   ('expr	  p-binary)
   ('xor_expr	  p-binary)
   ('and_expr	  p-binary)
   ('shift_expr	  p-binary)
   ('arith_expr	  p-binary)
   ('term	  p-binary)
   ('comparison	  p-binary)
   ('or_test	  p-binary)
   ('and_test	  p-binary)
   ('factor	  p-factor)
   ('power	  p-power)
   ('test	  p-one)
   ('not_test	  p-not-test)
   ('lambdef	  p-lambda)
   ('testlist	  p-testlist)
   ('exprlist	  p-testlist)
   ('expr_stmt	  p-binary)
   ('small_stmt	  p-one)
   ('simple_stmt  p-simple-stmt)
   ('stmt	  p-one)
   ('file_input	  p-file-input)
   ('compound_stmt p-one)
   ('funcdef	  p-funcdef)
   ('suite	  p-suite)
   ('flow_stmt	  p-one)
   ('if_stmt	  p-if-stmt)
   ('while_stmt	  p-while-stmt)
   ('for_stmt	  p-for-stmt)
   ('break_stmt	  p-simple)
   ('continue_stmt p-simple)
   ('pass_stmt	  p-simple)
   ('raise_stmt	  p-raise)
   ('return_stmt  p-return)
   ('atom	  p-atom)
   ))

(define p-expr
  (item:t  kind r val) -> {t='unparsed p=(params:unparsed kind) subs=(LIST (literal-string val r)) range=r}
  (item:nt kind r val) -> (match (alist/lookup parse-table kind) with
			     ;; not in the table, mark it as unparsed
			     (maybe:no) -> {t='unparsed p=(params:unparsed kind) subs=(p-list val) range=r}
			     ;; in the table - parse it and attach a range
			     (maybe:yes fun) -> (let ((n0 (fun val))) (%rset/range n0 r) n0)
			     ))


(define (pprint-node n d)
  ;;(print n.range)
  ;;(print-string "\t")
  (indent d)
  (print n.t)
  (print-string " ")
  (printn n.p)
  (for-each (lambda (n) (pprint-node n (+ d 1))) n.subs)
  )

(let ((t (if (> sys.argc 1) (parse sys.argv[1]) (parse "tests/parse_2.py"))))
  (printn t)
  (print-parse-tree t)
  (ppt t)
  (terpri)
  (let ((node (p-expr t)))
    (pprint-node node 0)
    node
    )
  )


