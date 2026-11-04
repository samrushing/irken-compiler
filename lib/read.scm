;; -*- Mode: Irken -*-

;; this is a new version of the lisp reader that includes source
;;  annotations.

;; temporary compatibility shim.

(define (reader path read-char)
  (let ((exps (read2 path read-char #t)))
    exps))

(define (read-string s)
  (reader "<string>" (string-reader s)))

(define (read2 path read-char parse-ints?)

  (let ((char #\eof) ;; one-character buffer
	(line 1)
	(pos -1)
        (line0 1)
        (pos0 -1)
        (spath (sexp:string path)))

    (defmacro ERR
      (ERR ob ...)
      -> (raise (:ReadError path line pos (format ob ...)))
      )

    (define (peek)
      char)

    (define (next)
      (let ((result char))
	(if (eq? result #\newline)
	    (begin (inc! line)
		   (set! pos 0))
	    (inc! pos))
	(set! char (read-char))
	result))

    (define (skip-peek)
      (next)
      (peek)
      )

    (define skip-comment
      #\return  -> #u
      #\newline -> #u
      _         -> (skip-comment (skip-peek))
      )

    (define (skip-whitespace)
      (let loop ((ch (peek)))
	(cond ((eq? ch #\eof) #u)
	      ((eq? ch #\;) (skip-comment (skip-peek)) (loop (peek)))
	      ((whitespace? ch) (loop (skip-peek)))
	      (else #u))))

    ;; very tricky, using a state machine
    (define (read-atom)
      (let ((state 0)
	    (dot-count 0)
	    (ch #\0))
	(let loop ((result '()))
	  (set! ch (peek))
	  (if (eq? ch #\.)
	      (set! dot-count (+ dot-count 1)))
	  (set! state
		(match state with
		  0 -> (cond ((eq? ch #\eof) 4)
			     ((eq? ch #\-) 1)
			     ((digit? ch) 2)
			     ((delim? ch) 7)
			     (else 3))
		  1 -> (cond ((eq? ch #\eof) 5)
			     ((delim? ch) 5)
			     ((digit? ch) 2)
			     (else 3))
		  2 -> (cond ((eq? ch #\eof) 6)
			     ((delim? ch) 6)
			     ((digit? ch) 2)
			     (else 3))
		  3 -> (cond ((eq? ch #\eof) 5)
			     ((delim? ch) 5)
			     (else 3))
		  _ -> (impossible)))
	  (cond ((< state 4) (loop (list:cons (next) result))) ;; non-final
		((= state 4) (ERR "unexpected end-of-file" path)) ;; error final
		(else ;; all other finals: 5,6,7
		 ;; single-character - for #\A
		 (if (= state 7) (set! result (list:cons (next) result)))
		 (:atom (list->string (reverse result)) ;; result string
			(= state 6)			;; number?
			(length result)			;; #chars
			dot-count))))))			;; #dots

    (define (make-src)
      (src:base {path=path line=line0 pos=pos0})
      )

    (define (dotted-symbol s n)
      ;; handle dots in a symbol
      ;; as a special case, allow all-dots symbols (like '...) through unscathed
      (if (= n (string-length s))
	  (sexp:symbol (string->symbol s) (make-src))
	  (let loop ((parts (reverse (string-split s #\.))))
	    ;; a.b.c => (get (get a b) c)
	    (match parts with
	      ;; c b a
	      (base)        -> (sexp:symbol (string->symbol base) (make-src))
	      ;;(attr . rest) -> (sexp (sym '%%attr) (loop rest) (sym (string->symbol attr)))
	      (attr . rest) -> (sexp:attr (loop rest) (string->symbol attr))
	      ()            -> (impossible)
	      ))))

    (define (read-symbol)
      (match (read-atom) with
	(:atom sym #t _ _) -> (ERR "expected symbol" (string sym))
	(:atom sym #f _ 0) -> (string->symbol sym)
	(:atom sym #f _ _) -> (ERR "no dots allowed in constructor names: " (string sym))
	))

    (define (src-location)
      ;;(sexp (cons 'nil 'tuple) (string path) (int line) (int pos))
      (sexp (sexp (sym '%%cons) (sym 'nil) (sym 'tuple)) (string path) (int line) (int pos))
      )

    (define (parse-char-code code)
      (let ((len (string-length code)))
        (match (string-ref code 0) with
          #\u -> (match (parse-integer 16 (substring code 1 len)) with
                   (result:ok val) -> (sexp:char (int->char val))
                   (result:err why) -> (ERR "bad character literal" (string code)))
          _ -> (ERR "bad character literal" (string code))
          )))

    (define (read1)
      (let ((ch (peek)))
	(match ch with
	  #\eof -> (ERR "unexpected end of file")
	  #\(   -> (sexp:list (read-list))
	  #\{   -> (read-record)
	  #\"   -> (read-string)
	  #\'   -> (begin (next) (sexp (sexp:symbol 'quote (make-src)) (read)))
	  #\,   -> (begin (next) (sexp (sexp:symbol 'comma (make-src)) (read)))
	  #\:   -> (begin (next) (sexp:cons 'nil (read-symbol)))
	  #\#   -> (begin
		     (next)
		     (set! ch (peek))
		     (match ch with
		       #\\ -> (begin
				(next) ;; skip backslash
				(match (read-atom) with
				  (:atom atom _ 1 _)      -> (sexp:char (string-ref atom 0))
				  (:atom "newline" _ _ _) -> (sexp:char #\newline)
				  (:atom "space" _ _ _)   -> (sexp:char #\space)
				  (:atom "return" _ _ _)  -> (sexp:char #\return)
				  (:atom "tab" _ _ _)     -> (sexp:char #\tab)
				  (:atom "eof" _ _ _)     -> (sexp:char #\eof)
				  (:atom "nul" _ _ _)     -> (sexp:char #\nul)
                                  (:atom code _ _ _)      -> (parse-char-code code)
				   ))
		       ;; Bb
		       #\X -> (begin (next) (read-integer-literal 16))
		       #\x -> (begin (next) (read-integer-literal 16))
		       #\O -> (begin (next) (read-integer-literal 8))
		       #\o -> (begin (next) (read-integer-literal 8))
                       #\B -> (begin (next) (read-integer-literal 2))
                       #\b -> (begin (next) (read-integer-literal 2))
		       #\T -> (begin (next) (sexp:bool #t))
		       #\t -> (begin (next) (sexp:bool #t))
		       #\F -> (begin (next) (sexp:bool #f))
		       #\f -> (begin (next) (sexp:bool #f))
		       #\U -> (begin (next) (sexp:undef))
		       #\u -> (begin (next) (sexp:undef))
                       #\W -> (begin (next) (src-location))
		       #\( -> (sexp:vector (read-list))
		       x   -> (ERR "syntax error: " (char x))
		       ))
	  #\)   -> (ERR "unexpected close-paren")
	  _     -> (match (read-atom) with
                     ;; XXX gaz: need to handle negative integers!
		     (:atom chars #t n _) -> (maybe-parse-integer 10 chars)
		     (:atom chars #f _ 0) -> (sexp:symbol (string->symbol chars) (make-src))
		     (:atom chars #f _ n) -> (dotted-symbol chars n)
		     )
	  )
	)
      )

    (define (read-postfix)
      (let ((result (read1)))
	(skip-whitespace)
	(let loop ((ch (peek)))
	  (match ch with
            ;; ------ postfix/infix extensions -----
	    ;; array-reference syntax
	    #\[ -> (let ((index (read-array-index)))
		     ;; primops take a parameter---------V
		     (set! result (sexp (symsrc '%array-ref (make-src)) (sexp:bool #f) result index))
                     (skip-whitespace)
                     (loop (peek)))
	    ;; infix colon syntax
	    #\: -> (let ((_ (next))
                         (left result)
                         (right (read)))
		     (match result right with
                       ;; normal constructor syntax
		       (sexp:symbol dt _) (sexp:symbol alt _) -> (sexpf (cons dt alt))
                       ;; object/method syntax (ob::add arg0 ...)
		       ;; not forcing (sexp:symbol) on <ob> might allow 'builtin method calls'...
		       ;;  ob (sexp:cons 'nil method) -> (sexp:attr (sexp:attr ob 'o) method)
                       ;;ob (sexp:cons 'nil method)
                       ob (sexp:cons 'nil method)
                       -> (sexp (sym '%%obref) ob (sym method))
                       ;; XXX note: maybe change to an %ann-otation.
		       ;; object : type syntax
		       ob type -> (sexp (sym '%typed) ob type)
		       ;;ob type -> (sexp (sexp:symbol '%typed) ob type)
                       ))
	    _   -> result
	    ))))

    (define (read-array-index)
      (next) ;; skip open-left-bracket
      (let ((exp (read)))
	(skip-whitespace)
	(if (eq? (peek) #\])
	    (begin (next) exp)
	    (ERR "expected closing ]/} character"))))

    (define (read-hex-digit ch)
      (match (alist/lookup hex-map ch) with
	(maybe:no) -> (ERR "bad hex digit: " (char ch))
	(maybe:yes num) -> num))

    (define (read-hex-code)
      (let ((n0 (read-hex-digit (next)))
	    (n1 (read-hex-digit (next))))
	(ascii->char (+ (<< n0 4) n1))))

    (define (read-string)
      (next) ;; throw away the opening quote
      (let loop ((ch (peek))
		 (result '()))
	(match ch with
	  #\" -> (begin
		   (next) ;; throw away the close-quote
		   (sexp:string (list->string (reverse result))))
	  #\\ -> (begin
		   ;; ignore this backslash, read the next char
		   (next)
		   (set! ch (next))
		   (match ch with
		     #\x -> (let ((sch (read-hex-code))) (loop (peek) (list:cons sch result)))
		     #\X -> (let ((sch (read-hex-code))) (loop (peek) (list:cons sch result)))
		     #\r -> (loop (peek) (list:cons #\return result))
		     #\n -> (loop (peek) (list:cons #\newline result))
		     #\t -> (loop (peek) (list:cons #\tab result))
		     #\" -> (loop (peek) (list:cons #\" result))
		     #\\ -> (loop (peek) (list:cons #\\ result))
		     _   -> (ERR "bad backslash escape in string: " (char ch))
		     ))
	  _   -> (loop (skip-peek) (list:cons ch result))
	  )))

    (define (read-list)
      (next) ;; throw away the open paren
      (let loop ((result '()))
	(skip-whitespace)
        (cond ((eq? (peek) #\))
               (next) ;; throw away the close paren
               (reverse result))
              (else
               (loop (list:cons (read) result))))))

    (define (read-record)
      ;; { label=value label=value ...}
      (next)
      (let loop ((result '()))
	(skip-whitespace)
	(let ((p (peek)))
	  (if (eq? p #\})
	      (begin (next) (sexp:record (reverse result)))
	      (let ((name (read-name)))
		(cond ((eq? name '...)
		       (loop (list:cons (field:t name (sexp:bool #f)) result)))
		      (else
		       (skip-whitespace)
		       (if (not (eq? (peek) #\=))
			   (ERR "expected '=' in record literal" (char (peek)))
			   (begin
			     (next)
			     (let ((val (read)))
			       (loop (list:cons (field:t name val) result))))))))
	      ))))

    (define (read-name)
      (let loop ((result '())
		 (ch (peek))
		 (dots #f))
	(cond ((or (field? ch) (eq? ch #\.))
	       (loop (list:cons ch result) (skip-peek) #f))
	      (else
	       (string->symbol (list->string (reverse result)))))))

    ;; ----- integer literals -----

    (define hex-digit? (char-class (string->list "0123456789ABCDEFabcdef")))
    (define oct-digit? (char-class (string->list "01234567")))
    (define bin-digit? (char-class (string->list "01")))

    (define (all-int-class? base s)
      (let ((chars (match (string->list s) with
                     (#\- . chars0) -> chars0
                     chars0 -> chars0)))
        (all?
         (match base with
           16 -> hex-digit?
           8  -> oct-digit?
           2  -> bin-digit?
           _  -> (impossible)
           )
         chars)))

    (define (maybe-parse-integer base val)
      (if parse-ints?
          (match (parse-integer base val) with
            (result:ok r) -> (sexp:int r)
            (result:err _) -> (ERR "error reading base " (int base) " integer:" (string val)))
          (sexp (sym '%int) (int base) (string val))))


    (define (read-integer-literal base)
      (match (read-atom) with
        (:atom chars dec? n 0)
        -> (if (all-int-class? base chars)
               (maybe-parse-integer base chars)
               (ERR "error reading base " (int base) " integer:" (string chars)))
        (:atom chars _ _ _)
        -> (ERR "unexpected char reading base " (int base)
                " literal:" (string chars))
        ))

    ;; ----------

    (define (read)
      (skip-whitespace)
      (set! line0 line)
      (set! pos0 pos)
      (read-postfix))

    (define (read-all acc)
      (if (eq? (peek) #\eof)
          (reverse acc)
          (read-all (list:cons (read) acc))))

    (next)
    (read-all '())
    ))
