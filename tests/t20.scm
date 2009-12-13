(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")
(include "lib/frb.scm")
(include "lib/symbol.scm")
(include "lib/io.scm")

(define eof-token (:token 'eof "eof"))

(define (lex producer consumer)
  ;; producer gives us characters
  ;; consumer takes tokens

  (let ((current (:nil))
	(action 'not-final)
	(good 'not-final)
	(state 0)
	)

      (define (final? action)
	(not (eq? action 'not-final)))

      ;; defines the <step> function (DFA) from the lexer generator
      (include "parse/lexstep.scm")

      (let loop ((ch (producer)))
	(cond ((char=? ch #\eof)
	       (consumer eof-token)
	       #t)
	      (else
	       (set! state (step ch state))
	       (set! action finals[state])
	       (cond ((and (not (final? good)) (final? action))
		      ;; we've entered a new final state
		      (set! good action)
		      (set! current (:cons ch current))
		      (loop (producer)))
		     ((and (final? good) (not (final? action)))
		      ;; we've left a final state - longest match - emit token
		      (consumer (:token good (list->string (reverse current))))
		      (set! current (:nil))
		      (set! state 0)
		      (set! good 'not-final)
		      (loop ch))
		     (else
		      ;; accumulate this character
		      (set! good action)
		      (set! current (:cons ch current))
		      (loop (producer)))))))
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

(include "parse/t0.scm")

(define (parse path)
  (let ((file (file:open-read path))
	(token-gen (make-lex-generator file))
	(paren-stack (:nil))
	(indentation 0)
	(start-of-line #t)
	(held-token eof-token)
	(tok eof-token)
	)
    
    (define (get-indent tok)
      (vcase tok
	((:token sym str)
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
		     (:token 'indent ""))
		    ((< this-indent indentation)
		     (set! indentation this-indent)
		     (:token 'dedent ""))
		    (else
		     (loop))))
	    ;; in the middle of a line somewhere
	    (vcase tok
	      ((:token sym str)
	       (case sym
		 ((NEWLINE)
		  (vcase paren-stack
		    ((:nil)
		     (set! start-of-line #t)
		     (:token 'NEWLINE ""))
		    ((:cons _ _) (loop))))
		 ((whitespace comment) (loop))
		 (else tok)))))
	))
    (let loop ((tok (next-token)))
      (printn tok)
      (if (eq? tok eof-token)
	  eof-token
	  (loop (next-token))))
    ))

(if (> (sys:argc) 1)
    (parse sys:argv[1])
    (parse "nodes.py"))
(printn actions)

;; (let ((f (file:open-read "nodes.py")))
;;   (define g (make-lex-generator f))
;;   (let loop ((tok (g)))
;;     (cond ((eq? tok eof-token) 23)
;; 	  (else
;;  	   (printn tok)
;; 	   (loop (g))))))

