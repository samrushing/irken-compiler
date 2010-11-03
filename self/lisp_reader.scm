;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")
(include "lib/io.scm")
(include "lib/alist.scm")
(include "lib/frb.scm")
(include "lib/symbol.scm")

(datatype sexp
  (:list (list (sexp)))
  (:symbol symbol)
  (:string string)
  (:char char)
  (:bool bool)
  (:int int)
  (:undef undefined)
  (:vector (list (sexp)))
  )

(define (reader file)

  (let ((char #\eof))  ;; one-character buffer

    (define (peek)
      (if (eq? char #\eof)
	  (set! char (file/read-char file))
	  #u)
      char)
    
    (define (next)
      (let ((result char))
	(set! char (file/read-char file))
	result))

    (define whitespace '(#\space #\tab #\newline #\return))
    (define digits     '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
    (define delimiters '(#\( #\) #\[ #\] #\{ #\}))
    (define hex-table
      (literal
       (alist/make
	(#\0 0) (#\1 1) (#\2 2) (#\3 3) (#\4 4) (#\5 5) (#\6 6) (#\7 7) (#\8 8) (#\9 9)
	(#\a 10) (#\b 11) (#\c 12) (#\d 13) (#\e 14) (#\f 15)
	(#\A 10) (#\B 11) (#\C 12) (#\D 13) (#\E 14) (#\F 15)
	)))

    (define (skip-comment)
      (let loop ((ch (next)))
	(match ch with
	  #\return -> #u
	  #\newline -> #u
	  _ -> (loop (next)))))

    (define (skip-whitespace)
      (let loop ((ch (peek)))
	(match ch with
	  #\eof     -> #u
	  #\;       -> (skip-comment)
	  #\space   -> (loop (next))
	  #\tab     -> (loop (next))
	  #\newline -> (loop (next))
	  #\return  -> (loop (next))
	  _         -> #u
	  )))

    (define (read-atom)
      (let ((result (LIST (next)))
	    (all-digits #t))
	(let loop ((result result))
	  (let ((ch (peek)))
	    (cond ((or (member? ch whitespace eq?) (member? ch delimiters eq?))
		   (:atom (list->string result) all-digits (length result)))
		  (else
		   (if (not (member? ch digits eq?)) (set! all-digits #f) #u)
		   (loop (list:cons (next) result)))
		  )))))

    (define (read)
      (skip-whitespace)
      (let ((ch (peek)))
	(match ch with
	  #\eof -> (error "unexpected end of file")
	  #\(   -> (sexp:list (read-list))
	  ;;#\{   -> (read-record)
	  #\"   -> (read-string)
	  #\'   -> (begin (next) (sexp:list (LIST (sexp:symbol 'quote) (read))))
	  #\,   -> (begin (next) (sexp:list (LIST (sexp:symbol 'comma) (read))))
	  #\:   -> (begin (next) (sexp:list (LIST (sexp:symbol 'colon) (read))))
	  #\#   -> (begin
		     (next)
		     (set! ch (peek))
		     (match ch with
		       #\\ -> (match (read-atom) with
				 (:atom atom #f 1)     -> (sexp:char (string-ref atom 0))
				 (:atom "newline" _ _) -> (sexp:char #\newline)
				 (:atom "space" _ _)   -> (sexp:char #\space)
				 (:atom "return" _ _)  -> (sexp:char #\return)
				 (:atom "tab" _ _)     -> (sexp:char #\tab)
				 (:atom "eof" _ _)     -> (sexp:char #\eof)
				 _                     -> (error "bad character constant")
				 )
		       ;; Xx Oo Bb
		       #\T -> (sexp:bool #t)
		       #\t -> (sexp:bool #t)
		       #\F -> (sexp:bool #f)
		       #\f -> (sexp:bool #f)
		       #\U -> (sexp:undef #u)
		       #\u -> (sexp:undef #u)
		       #\( -> (sexp:vector (read-list))
		       x   -> (error "syntax error")
		       ))
	  ;#\-   -> (sexp:int (- 0 (read-number)))
	  _     -> (match (read-atom) with
		     (:atom chars #t _) -> (sexp:int (parse-number chars))
		     (:atom chars #f _) -> (sexp:symbol (string->symbol chars))
		     )
	  )
	)
      )

    (define (parse-number x)
      ;; place-holder
      0)

    (define (read-hex-digit ch)
      (match (alist/lookup hex-table ch) with
	(maybe:no) -> (error "bad hex digit")
	(maybe:yes num) -> num))

    (define (read-hex-code)
      (let ((n0 (read-hex-digit (next)))
	    (n1 (read-hex-digit (next))))
	(ascii->char (+ (<< n0 8) n1))))

    (define (read-string)
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
		     #\x -> (loop (peek) (list:cons (read-hex-code) result))
		     #\X -> (loop (peek) (list:cons (read-hex-code) result))
		     #\n -> (loop (peek) (list:cons #\newline result))
		     #\t -> (loop (peek) (list:cons #\tab result))
		     _   -> (error "bad backslash escape in string")
		     ))
	  _   -> (loop (peek) (list:cons ch result))
	  )))
    
    (define (read-list)
      ;; throw away the open paren
      (next)
      (let loop ((ch #\eof)
		 (result '()))
	(skip-whitespace)
	(set! ch (peek))
	(if (eq? ch #\))
	    ;; throw away the paren
	    (begin (next) (reverse result))
	    (let ((exp (read)))
	      ;; XXX implement <include> here
	      (list:cons exp result))
	    )))

    (read)
    ))

(define (read-file path)
  (let ((file (file/open-read path)))
    (reader file)))

(let ((t (if (> sys.argc 1)
	     (read-file sys.argv[1])
	     (read-file "lib/core.scm"))))
  (printn t)
  )

