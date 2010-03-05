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
	(cond ((char=? ch #\eof) (consumer (token:t '<$> "<$>")#t))
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

(include "parse/t0.scm")

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
    
    (define (get-indent tok)
      (vcase token tok
	((:t sym str)
	 (if (eq? sym 'whitespace)
	     ;; XXX handle or disallow tabs
	     (string-length str)
	     ;; non-whitespace at the front of a line
	     0))))

    (define (next-token)
      ;; process (i.e., filter/synthesize) the token stream
      (let loop ()
	(cond ((not (eq? held-token eof-token))
	       (set! tok held-token)
	       (set! held-token eof-token))
	      (else
	       (set! tok (token-gen))))
	(if start-of-line
	    ;; in this state we might emit INDENT/DEDENT
	    (let ((this-indent (get-indent tok)))
	      (set! start-of-line #f)
	      (set! held-token tok)
	      (cond ((> this-indent indentation)
		     (set! indentation this-indent)
		     (token:t 'indent ""))
		    ((< this-indent indentation)
		     (set! indentation this-indent)
		     (token:t 'dedent ""))
		    (else
		     (loop))))
	    ;; in the middle of a line somewhere
	    (vcase token tok
	      ((:t sym str)
	       (case sym
		 ((newline)
		  (vcase list paren-stack
		    ((:nil)
		     (set! start-of-line #t)
		     (token:t 'newline ""))
		    ((:cons _ _) (loop))))
		 ((whitespace comment) (loop))
		 (else tok)))))
	))

    (let ((stack (stack:empty)))

      (define (get-state)
	(vcase stack stack
	  ((:empty) 0)
	  ((:elem _ state _) state)))
	   
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
        (vcase stack stack
          ((:empty) (error "stack underflow"))
          ((:elem item _ rest)
           (set! stack rest)
           item)))

      (let loop ((tok (next-token)))
	(cond ((eq? tok eof-token) (pop) (pop))
	      (else
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
	     (loop1 tail))))))))
      
(let ((t (if (> (sys:argc) 1) (parse sys:argv[1]) (parse "tests/parse_0.py"))))
  (print-parse-tree t)
  )

