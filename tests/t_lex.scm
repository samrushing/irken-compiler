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
	(cond ((char=? ch #\eof) (consumer eof-token) #t)
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
    (file/read-char file))

  (make-generator
   (lambda (consumer)
     (lex producer consumer)
     (let forever ()
       (consumer eof-token)
       (forever))
     )))

(let ((f (file/open-read "nodes.py")))
  (define g (make-lex-generator f))
  (let loop ((tok (g)))
    (cond ((eq? tok eof-token) 23)
	  (else
 	   (printn tok)
	   (loop (g))))))

