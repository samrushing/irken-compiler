;; -*- Mode: Irken -*-

;; test parser for a simple predicate language.
;; the grammar is defined in parse/t1.g, which generates parse/t1.scm
;; the python lexer is defined in parse/lexer.py, which generates parse/lexstep.scm
;; the sample file is in tests/parse_0.py
;;
;; the lexer produces tokens with attached line/column ranges, which are ignored here.

(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")
(include "lib/io.scm")

(include "parse/lexstep.scm")
(include "lib/lexer.scm")

;; parser tables

(include "parse/t1.scm")

;; stack = (:elem item state stack) | (:empty)
;; item  = (:nt kind (list (item 'a))) | (:t symbol)
;; args  = (:cons item args) | (:nil)
;; state = int
;; kind  = symbol

(datatype item
  (:nt symbol (range) (list (item)))
  (:t symbol (range) string)
  )

(datatype stack
  (:empty)
  (:elem (item) int (stack))
  )

(define (parse path)
  (let ((file (file/open-read path))
	(token-gen (make-lex-generator file))
	(paren-stack '())
	(indents (list 0))
	(start-of-line #t)
	(held-token eof-token)
	(tok eof-token)
	)
    
    (define get-indent
      ;; XXX handle or disallow tabs
      (token:t 'whitespace str _) -> (string-length str)
      ;; non-whitespace at the front of a line
      (token:t _ _ _ )            -> 0)

    (define (get-top-indent)
      (match indents with
        () -> 0
	(indent . _) -> indent))

    (define (next-token)
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

;;     (define (next-token)
;;       (let ((t (next-token0)))
;; 	(print-string "next-token: ") (printn t)
;; 	t))

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
	       ;(print-string "token: ") (printn tok)
	       ;(print-string "state: ") (printn (get-state))
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

(define (print-parse-tree t)

  (define (indent n)
    (let loop ((n n))
      (cond ((= n 0) #t)
	    (else
	     (print-string "  ")
	     (loop (- n 1))))))
  
  (let loop0 ((d 0)
	      (t t))
    (indent d)
    (vcase item t
      ((:t sym range str)
       (print sym) (print-string " ") (printn str))
      ((:nt sym range items)
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

;; AST

(datatype atom
  (:name string)
  (:number int)
  (:string string)
  )

(datatype expr
  (:pred string (list (expr)))
  (:atom (atom))
  )

;; parse-tree->AST

(define (p-file-input l)
  (let loop ((acc (list:nil))
	     (l l))
    (match l with
      ()                                                          -> acc
      ((item:nt _ _ ((item:t 'NEWLINE _ _))) (item:nt _ _ splat)) -> (loop acc splat) ;; ignore NEWLINE tokens
      ((item:nt _ _ (item0)) (item:nt _ _ splat))                 -> (loop (list:cons (p-expr item0) acc) splat)
      _ -> (error "p-file-input"))
    ))

(define p-atom
  (item:t 'NAME _ x)   -> (atom:name x)
  (item:t 'NUMBER _ x) -> (atom:number (string->int x))
  (item:t 'STRING _ x) -> (atom:string x)
  _ -> (error "p-atom")
  )

(define p-list
  (item:nt _ _ ((item:t 'comma _ _) expr recur)) -> (list:cons (p-expr expr) (p-list recur))
  (item:nt _ _ ())      -> (list:nil)
  _ -> (error "p-list2")
  )

(define p-args
  (item:nt _ _ ((item:nt 'list _ (expr rest)))) -> (list:cons (p-expr expr) (p-list rest))
  (item:nt _ _ ()) -> (list:nil)
  _ -> (error "p-args")
  )

(define p-expr2
  (item:nt 'predicate _ ((item:t 'NAME _ name) _ args _)) -> (expr:pred name (p-args args))
  (item:nt 'atom _ (x)) -> (expr:atom (p-atom x))
  _ -> (error "p-expr2")
  )

(define p-expr
  (item:nt 'expr _ (x)) -> (p-expr2 x)
  _ -> (error "p-expr")
  )

(define start
  (item:nt 'file _ val) -> (p-file-input val)
  _ -> (error "start"))

;; an unparser

(define print-expr
  (expr:pred name args) -> (begin (print-string name) (print-string " (") (print-args args) (print-string ")") #u)
  (expr:atom atom)      -> (print-atom atom))

(define print-args
  (arg0) -> (print-expr arg0)
  (arg0 . rest) -> (begin (print-expr arg0) (print-string ", ") (print-args rest))
  () -> (begin (print-string "") #u)
  )

(define print-atom
  (atom:string s) -> (begin (print-string s) #u)  ;; quotes are included
  (atom:number n) -> (begin (print-string (int->string n)) #u)
  (atom:name s)   -> (begin (print-string s) #u)
  )

(let ((t (if (> sys.argc 1) (parse sys.argv[1]) (parse "tests/parse_0.py"))))
  (printn t)
  (print-parse-tree t)
  ;;(ppt t)
  (terpri)
  (let ((parsed (start t)))
    ;;(printn parsed)
    (let loop ((l parsed))
      (match l with
	() -> #f
	(hd . tl) -> (begin (print-expr hd) (terpri) (loop tl))
	))
    ))

