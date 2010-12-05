;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")
(include "lib/io.scm")
(include "lib/alist.scm")
(include "lib/frb.scm")
(include "lib/symbol.scm")

(datatype field
  (:t string sexp)
  )

(datatype sexp
  (:list (list (sexp)))
  (:symbol symbol)
  (:string string)
  (:char char)
  (:bool bool)
  (:int int)
  (:undef undefined)
  (:vector (list (sexp)))
  (:record (list (field)))
  )

(define (char-class char-list)
  (let ((v (make-vector 256 #f)))

    (define (in-class? ch)
      v[(char->ascii ch)])

    (let loop ((l char-list))
      (match l with
	()        -> in-class?
	(hd . tl) -> (begin (set! v[(char->ascii hd)] #t) (loop tl))
	))))

(define (reader file)

  (let ((char #\eof)) ;; one-character buffer

    (define (peek)
      (if (eq? char #\eof)
	  (set! char (file/read-char file))
	  #u)
      char)
    
    (define (next)
      (let ((result char))
	;;(print result)
	(set! char (file/read-char file))
	result))

    (define (skip-peek)
      (next)
      (peek)
      )

    (define whitespace    '(#\space #\tab #\newline #\return))
    (define delimiters     (string->list "()[]{}:"))
    (define letters        (string->list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
    (define all-delimiters (append whitespace delimiters))
    (define digits         (string->list "0123456789"))

    (define whitespace?   (char-class '(#\space #\tab #\newline #\return)))
    (define delimiter?    (char-class all-delimiters))
    (define digit?        (char-class digits))
    (define letter?       (char-class letters))
    (define field?        (char-class (cons #\- (append letters digits))))

    (define hex-table
      (literal
       (alist/make
	(#\0 0) (#\1 1) (#\2 2) (#\3 3) (#\4 4) (#\5 5) (#\6 6) (#\7 7) (#\8 8) (#\9 9)
	(#\a 10) (#\b 11) (#\c 12) (#\d 13) (#\e 14) (#\f 15)
	(#\A 10) (#\B 11) (#\C 12) (#\D 13) (#\E 14) (#\F 15)
	)))

    (define dec-table
      (literal
       (alist/make
	(#\0 0) (#\1 1) (#\2 2) (#\3 3) (#\4 4) (#\5 5) (#\6 6) (#\7 7) (#\8 8) (#\9 9)
	)))
    
    (define (skip-comment)
      (let loop ((ch (next)))
	(match ch with
	  #\return -> #u
	  #\newline -> #u
	  _ -> (loop (skip-peek)))))

    (define (skip-whitespace)
      (let loop ((ch (peek)))
	(cond ((eq? ch #\eof) #u)
	      ((eq? ch #\;) (skip-comment) (loop (peek)))
	      ((whitespace? ch) (loop (skip-peek)))
	      (else #u))))

    (define (read-atom)
      (let ((all-digits? #t)
	    (after-first-char? #f))
	(let loop ((result '()))
	  (let ((ch (peek)))
	    (cond ((and after-first-char? (delimiter? ch))
		   (:atom (list->string (reverse result)) all-digits? (length result)))
		  (else
		   (set! after-first-char? #t)
		   (set! all-digits? (and all-digits? (digit? ch)))
		   (loop (list:cons (next) result))))))))

    (define (read1)
      (skip-whitespace)
      (let ((ch (peek)))
	(match ch with
	  #\eof -> (error "unexpected end of file")
	  #\(   -> (sexp:list (read-list))
	  #\{   -> (read-record)
	  #\"   -> (read-string)
	  #\'   -> (begin (next) (sexp:list (LIST (sexp:symbol 'quote) (read))))
	  #\,   -> (begin (next) (sexp:list (LIST (sexp:symbol 'comma) (read))))
	  #\:   -> (begin (next) (sexp:list (LIST (sexp:symbol 'colon) (read))))
	  #\#   -> (begin
		     (next)
		     (set! ch (peek))
		     (match ch with
		       #\\ -> (begin
				(next) ;; skip backslash
				(match (read-atom) with
				   (:atom atom _ 1)      -> (sexp:char (string-ref atom 0))
				   (:atom "newline" _ _) -> (sexp:char #\newline)
				   (:atom "space" _ _)   -> (sexp:char #\space)
				   (:atom "return" _ _)  -> (sexp:char #\return)
				   (:atom "tab" _ _)     -> (sexp:char #\tab)
				   (:atom "eof" _ _)     -> (sexp:char #\eof)
				   (:atom "nul" _ _)     -> (sexp:char #\nul)
				   _                     -> (error "bad character constant")
				   ))
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
	  ;;#\-   -> (sexp:int (- 0 (read-number)))
	  #\)   -> (error "unexpected close-paren")
	  _     -> (match (read-atom) with
		      (:atom chars #t n) -> (sexp:int (read-int chars n))
		      (:atom chars #f _) -> (sexp:symbol (string->symbol chars))
		      )
	  )
	)
      )

    (define (read)
      (let ((result (read1)))
	(skip-whitespace)
	(let ((ch (peek)))
	  (match ch with
	    ;; postfix array-reference syntax
	    #\[ -> (let ((index (read-array-index)))
		     (sexp:list (LIST (sexp:symbol '%%array-ref) result index)))
	    ;; infix colon syntax
	    #\: -> (begin
		     (next)
		     (let ((rhs (read)))
		       (sexp:list (LIST (sexp:symbol 'colon) result rhs))))
	    _   -> result
	    ))))

    (define (read-array-index)
      (next) ;; skip open-left-bracket
      (let ((exp (read)))
	(skip-whitespace)
	(if (eq? (peek) #\])
	    (begin (next) exp)
	    (error "expected closing ]/} character"))))
	   
    (define (read-hex-digit ch)
      (match (alist/lookup hex-table ch) with
	     (maybe:no) -> (error "bad hex digit")
	     (maybe:yes num) -> num))

    (define (read-hex-code)
      (let ((n0 (read-hex-digit (next)))
	    (n1 (read-hex-digit (next))))
	(ascii->char (+ (<< n0 8) n1))))

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
		     #\x -> (loop (peek) (list:cons (read-hex-code) result))
		     #\X -> (loop (peek) (list:cons (read-hex-code) result))
		     #\n -> (loop (peek) (list:cons #\newline result))
		     #\t -> (loop (peek) (list:cons #\tab result))
		     #\" -> (loop (peek) (list:cons #\" result))
		     #\\ -> (loop (peek) (list:cons #\\ result))
		     _   -> (error "bad backslash escape in string")
		     ))
	  _   -> (loop (skip-peek) (list:cons ch result))
	  )))
    
    (define (read-list)
      ;; throw away the open paren
      (next)
      (let loop ((result '()))
	(skip-whitespace)
	(let ((ch (peek)))
	  (if (eq? ch #\))
	      ;; throw away the paren
	      (begin (next) (reverse result))
	      (let ((exp (read)))
		;; XXX implement <include> here
		(loop (list:cons exp result)))
	      ))))

    (define (read-record)
      ;; { label=value label=value ...}
      (next)
      (let loop ((result '()))
	(skip-whitespace)
	(let ((p (peek)))
	  (if (eq? p #\})
	      (begin (next) (sexp:record (reverse result)))
	      (let ((name (read-name)))
		(skip-whitespace)
		(if (not (eq? (peek) #\=))
		    (error "expected '=' in record literal")
		    (begin
		      (next)
		      (let ((val (read)))
			(loop (list:cons (field:t name val) result))))))
	      ))))

    (define (read-name)
      (let loop ((result '())
		 (ch (peek)))
	(if (field? ch)
	    (loop (list:cons ch result) (skip-peek))
	    (list->string (reverse result)))))

    (define (read-int s n)
      (let loop ((i 0)
		 (r 0))
	(if (= i n)
	    r
	    (match (alist/lookup dec-table (string-ref s i)) with
	      (maybe:no) -> (error "bad decimal digit?")
	      (maybe:yes digit) -> (loop (+ i 1) (+ (* r 10) digit)))
	    )))

    (define (read-include path result)
      ;; cons the forms from this file onto result, in reverse order...
      (append (reverse (read-file path)) result))

    (define (read-all)
      (let loop ((result '()))
	(skip-whitespace)
	(if (eq? (peek) #\eof)
	    (reverse result)
	    (let ((form (read)))
	      (match form with
		(sexp:list ((sexp:symbol 'include) (sexp:string path))) -> (loop (read-include path result))
		_                                                       -> (loop (list:cons form result)))))))

    (read-all)
    ))

(define (read-file path)
  (print-string "reading file ") (printn path)
  (let ((file (file/open-read path)))
    (reader file)))

;; type hack so unread always returns int
(define (printi x)
  (print x)
  0)

(define unread-fields
  () -> #u
  ((field:t name val) . tl)
  -> (begin
       (print-string name)
       (print-string "=")
       (unread val)
       (print-string " ")
       (unread-fields tl)
       ))

(define unread-list
  ((sexp:symbol 'quote) ob) -> (begin (print-string "'") (unread ob))
  ((sexp:symbol 'colon) ob) -> (begin (print-string ":") (unread ob))
  ((sexp:symbol 'colon) ob0 ob1) -> (begin (unread ob0) (print-string ":") (unread ob1))
  l -> (begin (print-string "(") (for-each (lambda (x) (unread x) (print-string " ")) l) (print-string ")"))
  )

(define unread
  (sexp:list l)   -> (unread-list l)
  (sexp:symbol s) -> (printi s)
  (sexp:string s) -> (printi s)
  (sexp:char ch)  -> (printi ch)
  (sexp:bool #t)  -> (print-string "#t")
  (sexp:bool #f)  -> (print-string "#f")
  (sexp:int n)    -> (printi n)
  (sexp:undef _)  -> (printi #u)
  (sexp:vector v) -> (begin (print-string "#(") (for-each (lambda (x) (unread x) (print-string " ")) v) (print-string ")"))
  (sexp:record fl) -> (begin (print-string "{ ") (unread-fields fl) (print-string "}"))
  )

(let ((t (read-file
	  (if (> sys.argc 1)
	      sys.argv[1]
	      "lib/core.scm"))))
;;  (printn t)
  (for-each (lambda (x) (unread x) (newline)) t)
  #u
  )

