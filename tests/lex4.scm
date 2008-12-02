
(include "lib/core.scm")
(include "lib/string.scm")
(include "lib/io.scm")
(include "lib/pair.scm")
(include "lib/vector.scm")
(include "lib/symbol.scm")

;; Ok, there are a few nasty speed hacks in here.  but this code now
;;  runs within a factor 2 of the same code written by hand in C.
;;
;; [hand-written C]
;; $ time ./lex4
;; real	0m0.387s
;; user	0m0.386s
;; sys	0m0.001s
;;
;; [this file compiled by pxll]
;; $ time tests/lex4
;; #t
;; {total ticks: 1231003470 gc ticks: 2140272}
;; real	0m0.773s
;; user	0m0.765s
;; sys	0m0.008s
;;
;; hacks:
;;
;; 1) avoid lexical depth - try to keep oft-access variables as
;;  close to the inside of the loop as possible.
;;
;; 2) you can sneak some extra variables into a 'let loop', and not
;;  bother setting them when calling yourself recursively.  [can't
;;  decide if this is a bug or a feature. 8^)] this is only possible
;;  because the call is translated into a series of set! + goto...
;;
;; 3) the int-in-range %%cexp primitive.  I'm actually not sure this
;;  makes any difference once "gcc -O3" is done, but it feels good.

(define (int-in-range x lo hi)
  ;; range-test integer <x> in [lo,hi)
  (%%verify "TC_INT" 1 x)
  (%%verify "TC_INT" 1 lo)
  (%%verify "TC_INT" 1 hi)
  (%%cexp "PXLL_TEST((%s >= %s) && (%s < %s))" x lo x hi))

(define (lex filename)

  (let ((fd (open filename 0))
	(current '())
	(tokens '())
	(final #f)
	(last-final #f)
	(finals #f)
	(s #f)
	(slen 0)
	)

      ;; defines the <step> function (DFA) from the lexer generator (in parse/lexer.py)
      (include "tests/step.scm")

      ;; initialize the table of final states
      (set! finals (gen-finals))

      (set! s (read fd 12000))
      (set! slen (string-length s))
      (if (%zero? slen)
	  (begin (print-string "done?") #t)
	  (let char-loop ((i 0)
			  (n 1000)
			  (ch #f)
			  (ascii #f)
			  (state 0))
	    (if (%eq? n 0)
		#t
		(if (%eq? i slen)
		    (char-loop 0 (%- n 1))
		    (begin
		      (set! ch (string-ref s i))
		      (set! ascii (char->ascii ch))
		      (set! state (step ascii state))
		      (set! final (vector-ref finals state))
		      (if (and last-final (not final))
			  ;; transition out
			  (begin
			    (set! current '())
			    (set! last-final #f)
			    (set! state 0)
			    (char-loop i)
			    )
			  (begin
			    (set! last-final final)
			    (set! current (cons ch current))
			    (char-loop (%+ i 1)))))
		    ))))
      ;(reverse tokens)
      ))

(lex "parse/lexer.py")
