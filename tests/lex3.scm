
(include "lib/core.scm")
(include "lib/string.scm")
(include "lib/io.scm")
(include "lib/pair.scm")
(include "lib/vector.scm")
(include "lib/symbol.scm")

(define (int-in-range x lo hi)
  ;; range-text integer <x> in [lo,hi)
  (%%verify "TC_INT" 1 x)
  (%%verify "TC_INT" 1 lo)
  (%%verify "TC_INT" 1 hi)
  (%%cexp "(((%s >= %s) && (%s < %s)) ? PXLL_TRUE : PXLL_FALSE)" x lo x hi))

(define (lex filename)

  (let ((fd (open filename 0))
	(state 0)
	(current '())
	(tokens '())
	(final #f)
	(last-final #f)
	(finals #f)
	)

      (define (push-token)
	(print-string "PUSH TOKEN ")
	(%printn last-final)
	(print-string "  : ") (%print last-final) (print-string "\n")
	(set! tokens (cons (cons last-final (list->string (reverse current))) tokens))
	(print-string (list->string (reverse current)))
	(set! current '())
	(set! last-final #f)
	(set! state 0)
	)

      ;; defines the <step> function (DFA) from the lexer generator
      (include "tests/step.scm")

      ;; initialize the table of final states
      (set! finals (gen-finals))

      (let loop ((s (read fd 10000)))
	(%printn (string-length s))
	(if (%zero? (string-length s))
	    (begin (print-string "done?") #t)
	    (let loop2 ((n 0))
	      (print-string "[") (print-int n) (print-string ":") (%print last-final) (%print final) (print-string "]")
	      (if (%== n (string-length s))
		  (loop (read fd 1024))
		  (let ((ch (string-ref s n)))
		    (set! state (step (char->ascii ch) state))
		    (set! final (vector-ref finals state))
		    (%print ch) (print-string "->") (print-int state) (print-string " ")
		    (if (and last-final (not final))
			;; transition out
			(begin
			  (push-token)
			  (loop2 n)
			  )
			(begin
			  (set! last-final final)
			  (set! current (cons ch current))
			  (loop2 (%+ n 1)))))))))
      (if last-final
	  (push-token))
      (reverse tokens)
      ))

(lex "parse/lexer.py")
