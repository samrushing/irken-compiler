(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")
(include "lib/frb.scm")
(include "lib/symbol.scm")
(include "lib/io.scm")

(define (lex s)

  (let ((current (:nil))
	(entry (:no))
	(last-entry (:no))
	(ch #\0)
	(state 0)
	)

      (define (send-token kind)
	;(consumer (:token kind (list->string (reverse current))))
	(printn (:token kind (list->string (reverse current))))
	(set! current (:nil))
	(set! last-entry (:no)))

      ;; defines the <step> function (DFA) from the lexer generator
      (include "parse/lexstep.scm")

      (let char-loop ((i 0))
	(cond ((= i (string-length s)) #t)
	      (else
	       (set! ch (string-ref s i))
	       (set! state (step ch state))
	       (set! entry finals[state])
	       (vcase entry
		 ((:yes sym final)
		  (cond (final
			 ;; single-char final state
			 (send-token sym)
			 (set! state 0))
			(else
			 (set! last-entry entry)
			 (set! current (:cons ch current))))
		  (char-loop (+ i 1)))
		 ((:no)
		  (vcase last-entry
		    ;; multiple-char, must wait until transition out...
		    ((:yes sym final)
		     (send-token sym)
		     (set! state 0)
		     (char-loop i))
		    ((:no)
		     (set! last-entry entry)
		     (set! current (:cons ch current))
		     (char-loop (+ i 1)))
		    ))))))))

(lex "
def thing (x):
  return x + 5
")
