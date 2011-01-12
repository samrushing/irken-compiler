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

;; an s-expression datatype.

(datatype sexp
  (:list (list sexp))
  (:symbol symbol)
  (:string string)
  (:char char)
  (:bool bool)
  (:int int)
  (:undef)
  (:vector (list sexp))
  (:record (list field))
  (:cons symbol symbol) ;; constructor ':' syntax
  (:attr sexp symbol)	;; attribute '.' syntax
  )

;; In retrospect, I think it may have been a mistake to embed 'list'
;;   into sexp.  It forces all sexp-handling code to cover two cases,
;;   often triggering the need for an auxiliary function.  Might be
;;   cleaner to just have (sexp:nil) and (sexp:cons)...

;; similar to the list macro.  think of this as the 'list' function
;;   for s-expressions.
(defmacro sexp
  (sexp)       -> (sexp:list '())
  (sexp x ...) -> (sexp:list (LIST x ...))
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

(define (reader read-char)

  (let ((char #\eof)) ;; one-character buffer

    (define (peek)
      (if (eq? char #\eof)
	  (set! char (read-char))
	  #u)
      char)
    
    (define (next)
      (let ((result char))
	;;(print result)
	(set! char (read-char))
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

    (define whitespace?    (char-class '(#\space #\tab #\newline #\return)))
    (define delimiter?     (char-class all-delimiters))
    (define digit?         (char-class digits))
    (define letter?        (char-class letters))
    (define field?         (char-class (cons #\- (append letters digits))))

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
	    (dot-count 0)
	    (after-first-char? #f))
	(let loop ((result '()))
	  (let ((ch (peek)))
	    (cond ((and after-first-char? (or (eq? ch #\eof) (delimiter? ch)))
		   (:atom (list->string (reverse result)) all-digits? (length result) dot-count))
		  (else
		   (set! after-first-char? #t)
		   (set! all-digits? (and all-digits? (digit? ch)))
		   (if (eq? ch #\.) (set! dot-count (+ 1 dot-count)))
		   (loop (list:cons (next) result))))))))

    (define (dotted-symbol s n)
      ;; handle dots in a symbol
      ;; as a special case, allow all-dots symbols (like '...) through unscathed
      (if (= n (string-length s))
	  (sexp:symbol (string->symbol s))
	  (let loop ((parts (reverse (string-split s #\.))))
	    ;; a.b.c => (get (get a b) c)
	    (match parts with
	      ;; c b a
	      (base)        -> (sexp:symbol (string->symbol base))
	      (attr . rest) -> (sexp:attr (loop rest) (string->symbol attr))
	      ()            -> (impossible)
	      ))))

    (define (read-symbol)
      (match (read-atom) with
	(:atom sym #t _ _) -> (error1 "expected symbol" sym)
	(:atom sym #f _ 0) -> (string->symbol sym)
	(:atom sym #f _ _) -> (error1 "no dots allowed in constructor names" sym)
	))

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
				   x                       -> (error1 "bad character constant" x)
				   ))
		       ;; Xx Oo Bb
		       #\T -> (begin (next) (sexp:bool #t))
		       #\t -> (begin (next) (sexp:bool #t))
		       #\F -> (begin (next) (sexp:bool #f))
		       #\f -> (begin (next) (sexp:bool #f))
		       #\U -> (begin (next) (sexp:undef))
		       #\u -> (begin (next) (sexp:undef))
		       #\( -> (sexp:vector (read-list))
		       x   -> (error1 "syntax error" x)
		       ))
	  #\)   -> (error "unexpected close-paren")
	  _     -> (match (read-atom) with
		      (:atom chars #t n _) -> (sexp:int (read-int chars n))
		      (:atom chars #f _ 0) -> (sexp:symbol (string->symbol chars))
		      (:atom chars #f _ n) -> (dotted-symbol chars n)
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
		     (match result with
		       (sexp:symbol dt) -> (sexp:cons dt (read-symbol))
		       _ -> (error1 "colon follows non-datatype" result)))
	    ;; infix 'get' syntax (i.e., attribute access)
	    ;; XXX this is disabled because it breaks symbols like '...
	    ;;   so we'll probably need to do the same hack as the python version
	    ;;#\. -> (begin (next) (sexp:attr result (read-symbol)))
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
    (reader (lambda () (file/read-char file)))))

(define (read-string s)
  (reader (string-reader s)))

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
  () -> (print-string "()")
   l -> (begin (print-string "(")
	       (print-sep unread " " l)
	       (print-string ")")
	       )
   )

(define unread
  (sexp:list l)     -> (unread-list l)
  (sexp:symbol s)   -> (print s)
  (sexp:string s)   -> (print s)
  (sexp:char ch)    -> (print ch)
  (sexp:bool #t)    -> (print-string "#t")
  (sexp:bool #f)    -> (print-string "#f")
  (sexp:int n)      -> (print n)
  (sexp:undef)      -> (print #u)
  (sexp:vector v)   -> (begin (print-string "#(") (for-each (lambda (x) (unread x) (print-string " ")) v) (print-string ")"))
  (sexp:record fl)  -> (begin (print-string "{ ") (unread-fields fl) (print-string "}"))
  (sexp:cons dt c)  -> (begin (if (eq? dt 'nil) #u (print dt)) (print-string ":") (print c))
  (sexp:attr lhs a) -> (begin (unread lhs) (print-string ".") (print a))
  )

(define (test-file)
  (let ((t (read-file
	    (if (> sys.argc 1)
		sys.argv[1]
		"lib/core.scm"))))
    ;;  (printn t)
    (for-each (lambda (x) (printn x) (unread x) (newline)) t)
    #u
    ))

(define (test-string)
  (for-each
   (lambda (x) (printn x) (unread x) (newline))
   ;;(read-string "(testing one two 0 1 #\\newline (\"string\" . xxx))")
   (read-string "(datatype list (:cons 'a (list 'a)) (:nil))")
   ))

;;(test-lisp-reader)
;;(test-string)

