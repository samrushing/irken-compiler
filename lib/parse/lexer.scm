;; -*- Mode: Irken -*-

(datatype range
  ;; range within a source file
  ;; <line0> <pos0> <line1> <pos1>
  (:t int int int int)
  ;; undefined range
  (:f)
  )

(typealias token {kind=symbol val=string range=range})

;; run-length encoding is used by the lexer generator to build
;; compact DFA tables.
(datatype rle
  (:run int int) ;; count val
  (:one int)     ;; val
  )

(defmacro rle*
  (rle* (acc ...) ())
  -> (rle->vector (reverse (list acc ...)))
  (rle* (acc ...) ((n v) rest ...))
  -> (rle* ((rle:run n v) acc ...) (rest ...))
  (rle* (acc ...) (v rest ...))
  -> (rle* ((rle:one v) acc ...) (rest ...))
  )

(defmacro rle
  (rle ob ...)
  -> (rle* () (ob ...))
  )

;; take a list of rle and turn it into an int[256]
(define (rle->vector runs)
  (let ((vec (make-vector 256 0))
        (i 0))
    (for-list run runs
      (match run with
        (rle:one val)
        -> (begin (set! vec[i] val) (set! i (+ i 1)))
        (rle:run n val)
        -> (for-range j n
             (set! vec[i] val)
             (set! i (+ i 1)))
        ))
    vec))

(define (lex dfa producer consumer)
  ;; producer gives us characters
  ;; consumer takes tokens

  (let ((action 'not-final)
	(state 0)
	;; current char's position
	(line 1)
	(pos 1)
	;; previous char's position
	(lline 1)
	(lpos 1)
	;; start of last token
	(tline 1)
	(tpos 1)
	)

    (define (next-char)
      ;; manage line/pos
      (let ((ch (producer)))
	(set! lline line)
	(set! lpos pos)
	(cond ((char=? ch #\newline)
	       (set! line (+ line 1))
	       (set! pos 1))
	      (else
	       (set! pos (+ pos 1))))
	ch))

    (define (final? action)
      (not (eq? action 'not-final)))

    (define (emit tok)
      (consumer (maybe:yes tok)))

    (let loop ((ch (next-char))
	       (last 'not-final)
	       (current (list:nil)))
      ;; (printf "state " (int state) " ch " (char ch) "\n")
      (cond ((char=? ch #\eof)
             (when (> (length current) 0)
               ;; tricky: when the file does not end with a newline,
               ;;  we are left with a token 'in the pipe'. If so, emit it
               ;;  before going home.  This is probably *wrong* for files
               ;;  that just happen to end in partial tokens.  Will probably
               ;;  revisit this.
               (emit {kind=last
                      val=(list->string (reverse current))
                      range=(range:t tline tpos lline lpos)}))
	     (emit {kind='EOF val="" range=(range:t tline tpos line pos)}))
	    (else
	     (set! state (dfa.step ch state))
	     (set! action dfa.finals[state])
	     (cond ((and (not (final? last)) (final? action))
		    ;; we've entered a new final state
		    (loop (next-char) action (list:cons ch current)))
		   ;;((and (final? last) (not (final? action)))
		   ((and (final? last) (eq? action '%%sink%%))
                    ;; (printf "exit final to " (int state) "\n")
		    ;; we've left a final state - longest match - emit token
		    (emit {kind=last
                           val=(list->string (reverse current))
                           range=(range:t tline tpos lline lpos)})
		    (set! state 0)
		    (set! tline lline)
		    (set! tpos lpos)
		    (loop ch 'not-final (list:nil)))
		   (else
		    ;; accumulate this character
		    (loop (next-char) action (list:cons ch current)))))))
      ))

(define (make-lex-generator dfa chargen)

  ;; this shim is only needed because 'lex' above isn't expecting
  ;;   a maybe, but a char.  should probably change that.
  (define (producer)
    (match (chargen) with
      (maybe:yes ch) -> ch
      (maybe:no) -> #\eof
      ))

  (make-generator
   (lambda (consumer)
     (lex dfa producer consumer)
     (forever (consumer (maybe:no)))
     )))

