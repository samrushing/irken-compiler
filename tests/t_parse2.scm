(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")
(include "lib/frb.scm")
(include "lib/symbol.scm")
(include "lib/io.scm")

(datatype token
  ;;  <kind> <value>
  (:t symbol string)
  )

(define eof-token (token:t 'eof "eof"))

(define (lex producer consumer)
  ;; producer gives us characters
  ;; consumer takes tokens

  (let ((action 'not-final)
	(state 0))

      (define (final? action)
	(not (eq? action 'not-final)))

      ;; defines the <step> function (DFA) from the lexer generator
      (include "parse/lexstep.scm")

      (let loop ((ch (producer))
		 (last 'not-final)
		 (current (list:nil)))
	(cond ((char=? ch #\eof)
	       ;; parser seems to require an extra NEWLINE in here...
	       (consumer (token:t 'NEWLINE "\n"))
	       (consumer (token:t '<$> "<$>")) #t)
	      (else
	       (set! state (step ch state))
	       (set! action finals[state])
	       (cond ((and (not (final? last)) (final? action))
		      ;; we've entered a new final state
		      (loop (producer) action (list:cons ch current)))
		     ((and (final? last) (not (final? action)))
		      ;; we've left a final state - longest match - emit token
		      (consumer (token:t last (list->string (reverse current))))
		      (set! state 0)
		      (loop ch 'not-final (list:nil)))
		     (else
		      ;; accumulate this character
		      (loop (producer) action (list:cons ch current)))))))
      ))

(define (make-lex-generator file)

  (define (producer)
    (file:read-char file))

  (make-generator
   (lambda (consumer)
     (lex producer consumer)
     (let forever ()
       (consumer eof-token)
       (forever))
     )))

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

;; stack = (:elem item state stack) | (:empty)
;; item  = (:nt kind (list (item 'a))) | (:t symbol)
;; args  = (:cons item args) | (:nil)
;; state = int
;; kind  = symbol

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
	(indentation 0)
	(start-of-line #t)
	(held-token eof-token)
	(tok eof-token)
	)
    
    (define get-indent
      ;; XXX handle or disallow tabs
      (token:t 'whitespace str) -> (string-length str)
      ;; non-whitespace at the front of a line
      (token:t _ _)             -> 0)

    (define (next-token)
      ;; process (i.e., filter/synthesize) the token stream
      (let loop ()
	(cond ((not (eq? held-token eof-token))
	       (set! tok held-token)
	       (set! held-token eof-token))
	      (else
	       (set! tok (token-gen))
	       ;(print "token-gen: ") (printn tok)
	       ))
	;(print "next-token loop ") (printn start-of-line)
	(if start-of-line
	    ;; in this state we might emit INDENT/DEDENT
	    (let ((this-indent (get-indent tok)))
	      (set! start-of-line #f)
	      (set! held-token tok)
	      (cond ((> this-indent indentation)
		     (set! indentation this-indent)
		     (token:t 'INDENT ""))
		    ((< this-indent indentation)
		     (set! indentation this-indent)
		     ;; urgh, we have to emit DEDENT *and* NEWLINE
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

    (let ((stack (stack:empty)))

      (define (get-state)
	(match stack with
	  (stack:empty)          -> 0
	  (stack:elem _ state _) -> state
	  ))

      (define (lookup-action state kind)
	(let loop ((l actions[state]))
	  (vcase action-list l
	     ((:nil) (error "missing action?"))
	     ((:cons tkind action tl)
	      (if (eq? terminals[tkind] kind)
		  action
		  (loop tl))))))

      (define (lookup-goto state nt)
	(let loop ((l goto[state]))
	  (vcase goto-list l
	     ((:nil) (error "missing goto?"))
	     ((:cons nt0 new-state tl)
	      (if (eq? nt0 nt)
		  new-state
		  (loop tl))))))

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
	       (print-string "token: ") (printn tok)
	       (print-string "state: ") (printn (get-state))
	       ;;(print "indentation: ") (printn indentation)
	       (vcase token tok
		 ((:t kind val)
		  (let ((a (lookup-action (get-state) kind)))
		    (print "action ") (printn a)
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

;; return a list of strings resembling a byte-code
(define compile
  acc (item:nt 'atom ((item:t _ val))) -> (list:cons val acc)

  ;; how to tell if a node is populated or 'empty'?
  ;; you look here:
  ;;                                      VVVVV
  acc (item:nt 'arith_expr (a (item:nt _ (b c _))))
  ;; if there are three items it is populated. if there are two it is not.
  ;; the last item is always (item:nt _ ()) representing empty production rule.
  -> (list:cons "PLUS"  (compile (compile acc c) a))

  acc (item:nt 'term (a b))            -> (list:cons "TIMES" (compile (compile acc a) b))
  acc (item:nt _ x) -> (compile-list acc x)
  acc _ -> acc
  )

(define compile-list
  acc ()      -> acc
  acc (x . y) -> (compile (compile-list acc y) x)
  )

(let ((t (if (> (sys:argc) 1) (parse sys:argv[1]) (parse "tests/parse_2.py"))))
  (printn t)
  (print-parse-tree t)
  (ppt t)
  (terpri)
  ;;(c-file-input t)
  (compile (list:nil) t)
  )

