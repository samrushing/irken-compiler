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
	(pos -1)) ;; column of the character in `char`, counting from 0

    (defmacro ERR
      (ERR ob ...)
      -> (raise (:ReadError path line (+ pos 1) (format ob ...)))
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

    ;; the location of the next character (columns count from 1).
    (define (here)
      (src:base {path=path line=line pos=(+ pos 1)}))

    (define (dotted-symbol s n src)
      ;; handle dots in a symbol
      ;; as a special case, allow all-dots symbols (like '...) through unscathed
      (if (= n (string-length s))
	  (sexp:symbol (string->symbol s) src)
	  (let loop ((parts (reverse (string-split s #\.))))
	    ;; a.b.c => (get (get a b) c)
	    (match parts with
	      ;; c b a
	      (base)        -> (sexp:symbol (string->symbol base) src)
	      ;;(attr . rest) -> (sexp (sym '%%attr) (loop rest) (sym (string->symbol attr)))
	      (attr . rest) -> (sexp:attr (loop rest) (string->symbol attr) src)
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
      (sexp (sexp (sym '%%cons) (sym 'nil) (sym 'tuple)) (string path) (int line) (int (+ pos 1)))
      )

    (define (parse-char-code code src)
      (let ((len (string-length code)))
        (match (string-ref code 0) with
          #\u -> (match (parse-integer 16 (substring code 1 len)) with
                   (result:ok val) -> (sexp:char (int->char val) src)
                   (result:err why) -> (ERR "bad character literal" (string code)))
          _ -> (ERR "bad character literal" (string code))
          )))

    (define (read1)
      (let ((ch (peek))
            (src (here)))
	(match ch with
	  #\eof -> (ERR "unexpected end of file")
	  #\(   -> (sexp:list (read-list) src)
	  #\{   -> (read-record src)
	  #\"   -> (read-string src)
	  #\'   -> (begin (next) (sexp:list (list (sexp:symbol 'quote src) (read)) src))
	  #\,   -> (begin (next) (sexp:list (list (sexp:symbol 'comma src) (read)) src))
	  #\:   -> (begin (next) (sexp:cons 'nil (read-symbol) src))
	  #\#   -> (begin
		     (next)
		     (set! ch (peek))
		     (match ch with
		       #\\ -> (begin
				(next) ;; skip backslash
				(match (read-atom) with
				  (:atom atom _ 1 _)      -> (sexp:char (string-ref atom 0) src)
				  (:atom "newline" _ _ _) -> (sexp:char #\newline src)
				  (:atom "space" _ _ _)   -> (sexp:char #\space src)
				  (:atom "return" _ _ _)  -> (sexp:char #\return src)
				  (:atom "tab" _ _ _)     -> (sexp:char #\tab src)
				  (:atom "eof" _ _ _)     -> (sexp:char #\eof src)
				  (:atom "nul" _ _ _)     -> (sexp:char #\nul src)
                                  (:atom code _ _ _)      -> (parse-char-code code src)
				   ))
		       ;; Bb
		       #\X -> (begin (next) (read-integer-literal 16 src))
		       #\x -> (begin (next) (read-integer-literal 16 src))
		       #\O -> (begin (next) (read-integer-literal 8 src))
		       #\o -> (begin (next) (read-integer-literal 8 src))
                       #\B -> (begin (next) (read-integer-literal 2 src))
                       #\b -> (begin (next) (read-integer-literal 2 src))
		       #\T -> (begin (next) (sexp:bool #t src))
		       #\t -> (begin (next) (sexp:bool #t src))
		       #\F -> (begin (next) (sexp:bool #f src))
		       #\f -> (begin (next) (sexp:bool #f src))
		       #\U -> (begin (next) (sexp:undef src))
		       #\u -> (begin (next) (sexp:undef src))
                       #\W -> (begin (next) (src-location))
		       #\( -> (sexp:vector (read-list) src)
		       x   -> (ERR "syntax error: " (char x))
		       ))
	  #\)   -> (ERR "unexpected close-paren")
	  _     -> (match (read-atom) with
                     ;; XXX gaz: need to handle negative integers!
		     (:atom chars #t n _) -> (maybe-parse-integer 10 chars src)
		     (:atom chars #f _ 0) -> (sexp:symbol (string->symbol chars) src)
		     (:atom chars #f _ n) -> (dotted-symbol chars n src)
		     )
	  )
	)
      )

    ;; postfix forms (x[i], dt:alt, ob::method, x:type) are at their left operand.
    (define (read-postfix)
      (let ((src (here))
            (result (read1)))
	(skip-whitespace)
	(let loop ((ch (peek)))
	  (match ch with
            ;; ------ postfix/infix extensions -----
	    ;; array-reference syntax
	    #\[ -> (let ((index (read-array-index)))
		     ;; primops take a parameter---------V
		     (set! result (sexp:list (list (sexp:symbol '%array-ref src) (sexp:bool #f src) result index) src))
                     (skip-whitespace)
                     (loop (peek)))
	    ;; infix colon syntax
	    #\: -> (let ((_ (next))
                         (left result)
                         (right (read)))
		     (match result right with
                       ;; normal constructor syntax
		       (sexp:symbol dt _) (sexp:symbol alt _) -> (sexp:cons dt alt src)
                       ;; object/method syntax (ob::add arg0 ...)
		       ;; not forcing (sexp:symbol) on <ob> might allow 'builtin method calls'...
		       ;;  ob (sexp:cons 'nil method) -> (sexp:attr (sexp:attr ob 'o) method)
                       ;;ob (sexp:cons 'nil method)
                       ob (sexp:cons 'nil method _)
                       -> (sexp:list (list (sexp:symbol '%%obref src) ob (sexp:symbol method src)) src)
                       ;; XXX note: maybe change to an %ann-otation.
		       ;; object : type syntax
		       ob type -> (sexp:list (list (sexp:symbol '%typed src) ob type) src)
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

    (define (read-string src)
      (next) ;; throw away the opening quote
      (let loop ((ch (peek))
		 (result '()))
	(match ch with
	  #\" -> (begin
		   (next) ;; throw away the close-quote
		   (sexp:string (list->string (reverse result)) src))
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

    (define (read-record src)
      ;; { label=value label=value ...}
      (next)
      (let loop ((result '()))
	(skip-whitespace)
	(let ((p (peek)))
	  (if (eq? p #\})
	      (begin (next) (sexp:record (reverse result) src))
	      (let ((fsrc (here))
                    (name (read-name)))
		(cond ((eq? name '...)
		       (loop (list:cons (field:t name (sexp:bool #f fsrc) fsrc) result)))
		      (else
		       (skip-whitespace)
		       (if (not (eq? (peek) #\=))
			   (ERR "expected '=' in record literal" (char (peek)))
			   (begin
			     (next)
			     (let ((val (read)))
			       (loop (list:cons (field:t name val fsrc) result))))))))
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

    (define (maybe-parse-integer base val src)
      (if parse-ints?
          (match (parse-integer base val) with
            (result:ok r) -> (sexp:int r src)
            (result:err why)
            -> (match why with
                 (:IntegerTooBig _)
                 -> (ERR "integer literal too big (irken integers are 63 bits): " (string val))
                 (:BadInteger _)
                 -> (ERR "error reading base " (int base) " integer:" (string val))))
          (sexp:list (list (sexp:symbol '%int src) (sexp:int base src) (sexp:string val src)) src)))


    (define (read-integer-literal base src)
      (match (read-atom) with
        (:atom chars dec? n 0)
        -> (if (all-int-class? base chars)
               (maybe-parse-integer base chars src)
               (ERR "error reading base " (int base) " integer:" (string chars)))
        (:atom chars _ _ _)
        -> (ERR "unexpected char reading base " (int base)
                " literal:" (string chars))
        ))

    ;; ----------

    (define (read)
      (skip-whitespace)
      (read-postfix))

    (define (read-all acc)
      (if (eq? (peek) #\eof)
          (reverse acc)
          (read-all (list:cons (read) acc))))

    (next)
    (read-all '())
    ))
