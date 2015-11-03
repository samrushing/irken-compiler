;; -*- Mode: Irken -*-

(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")
(include "lib/io.scm")
(include "lib/os.scm")
(include "lib/alist.scm")
(include "lib/aa_map.scm")
(include "lib/symbol.scm")

;; XXX consider rewriting with more experience

(datatype field
  (:t symbol sexp)
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

;; idea: how about a set of macros, similar to the format macro,
;;   to make sexps easier to build?  worth it?

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

(define hex-map
  (literal
   (alist/make
    (#\0 0) (#\1 1) (#\2 2) (#\3 3) (#\4 4) (#\5 5) (#\6 6) (#\7 7) (#\8 8) (#\9 9)
    (#\a 10) (#\b 11) (#\c 12) (#\d 13) (#\e 14) (#\f 15)
    (#\A 10) (#\B 11) (#\C 12) (#\D 13) (#\E 14) (#\F 15)
    )))

(define dec-map
  (literal
   (alist/make
    (#\0 0) (#\1 1) (#\2 2) (#\3 3) (#\4 4) (#\5 5) (#\6 6) (#\7 7) (#\8 8) (#\9 9)
    )))

(define oct-map
  (literal
   (alist/make
    (#\0 0) (#\1 1) (#\2 2) (#\3 3) (#\4 4) (#\5 5) (#\6 6) (#\7 7)
    )))

(define whitespace    '(#\space #\tab #\newline #\return))
(define delimiters     (string->list "()[]{}:"))
(define letters        (string->list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(define all-delimiters (append whitespace delimiters))
(define digits         (string->list "0123456789"))

(define whitespace?    (char-class '(#\space #\tab #\newline #\return)))
(define delim?         (char-class all-delimiters))
(define digit?         (char-class digits))
(define letter?        (char-class letters))
(define field?         (char-class (cons #\- (append letters digits))))

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
		((= state 4) (error "unexpected end-of-file")) ;; error final
		(else ;; all other finals: 5,6,7
		 ;; single-character - for #\A
		 (if (= state 7) (set! result (list:cons (next) result)))
		 (:atom (list->string (reverse result)) ;; result string
			(= state 6)			;; number?
			(length result)			;; #chars
			dot-count))))))			;; #dots

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
	  #\'   -> (begin (next) (sexp (sexp:symbol 'quote) (read)))
	  #\,   -> (begin (next) (sexp (sexp:symbol 'comma) (read)))
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
		       ;; Bb
		       #\X -> (begin (next) (sexp:int (read-hex-int)))
		       #\x -> (begin (next) (sexp:int (read-hex-int)))
		       #\O -> (begin (next) (sexp:int (read-oct-int)))
		       #\o -> (begin (next) (sexp:int (read-oct-int)))
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
		     ;; primops take a parameter---------V
		     (sexp (sexp:symbol '%array-ref) (sexp:bool #f) result index))
	    ;; infix colon syntax
	    #\: -> (begin
		     (next)
		     (match result (read) with
		       (sexp:symbol dt) (sexp:symbol alt) -> (sexp:cons dt alt)
		       ;; not forcing (sexp:symbol) on <ob> might allow 'builtin method calls'...
		       ;;ob (sexp:cons 'nil method) -> (sexp:attr (sexp:attr ob 'o) method)
		       ob (sexp:cons 'nil method) -> (sexp (sexp:symbol '%method) (sexp:symbol method) ob)
		       ;; object : type syntax
		       ob type -> (sexp (sexp:symbol '%typed) ob type)))
		       ;;x y -> (error1 "colon syntax" (:pair x y))))
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
      (match (alist/lookup hex-map ch) with
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
		     #\r -> (loop (peek) (list:cons #\return result))
		     #\n -> (loop (peek) (list:cons #\newline result))
		     #\t -> (loop (peek) (list:cons #\tab result))
		     #\" -> (loop (peek) (list:cons #\" result))
		     #\\ -> (loop (peek) (list:cons #\\ result))
		     _   -> (error1 "bad backslash escape in string" result)
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
		;; XXX should I check for <include> here?
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
		(cond ((eq? name '...)
		       (loop (list:cons (field:t name (sexp:bool #f)) result)))
		      (else
		       (skip-whitespace)
		       (if (not (eq? (peek) #\=))
			   (error1 "expected '=' in record literal" result)
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

    (define (read-int s n)
      (let ((neg? (eq? (string-ref s 0) #\-))
	    (start (if neg? 1 0)))
	(let loop ((i start)
		   (r 0))
	  (if (= i n)
	      (if neg? (- 0 r) r)
	      (match (alist/lookup dec-map (string-ref s i)) with
		(maybe:no) -> (error1 "bad decimal digit?" s)
		(maybe:yes digit) -> (loop (+ i 1) (+ (* r 10) digit)))
	      ))))

    (define (read-hex-int)
      (let ((neg? (eq? (peek) #\-)))
	(if neg? (begin (next) #u))
	(let loop ((r 0) (ch (peek)))
	  (match (alist/lookup hex-map ch) with
	    (maybe:yes digit) -> (loop (+ (* r 16) digit) (skip-peek))
	    (maybe:no) -> (if neg? (- 0 r) r)))))

    (define (read-oct-int)
      (let ((neg? (eq? (peek) #\-)))
	(if neg? (begin (next) #u))
	(let loop ((r 0) (ch (peek)))
	  (match (alist/lookup oct-map ch) with
	    (maybe:yes digit) -> (loop (+ (* r 8) digit) (skip-peek))
	    (maybe:no) -> (if neg? (- 0 r) r)))))

    (define (read-include path result)
      ;; cons the forms from this file onto result, in reverse order...
      (append (reverse (find-and-read-file path)) result))

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

(define (join-paths a b)
  (let ((alen (string-length a)))
    (if (char=? #\/ (string-ref a (- alen 1)))
	(format a b)
	(format a "/" b))))

(define find-file
  () name
  -> (raise (:FileNotFound "file not found" name))
  (dir . dirs) name
  -> (try
      (let ((name0 (join-paths dir name)))
	(when the-context.options.verbose
	      (printf "trying " name0 "\n"))
	(file/open-read name0))
      except
      (:OSError _) -> (find-file dirs name)
      ))

(define (read-file path)
  (print-string "reading file ") (printn path)
  (let ((file (file/open-read path)))
    (reader (lambda () (file/read-char file)))))

(define (find-and-read-file path)
  (print-string "reading file ") (printn path)
  (let ((file (find-file the-context.options.include-dirs path)))
    (reader (lambda () (file/read-char file)))))

(define (read-string s)
  (reader (string-reader s)))

(define sexp->symbol
  (sexp:symbol s) -> s
  x -> (error1 "sexp->symbol" x))

(define sexp->int
  (sexp:int n) -> n
  x -> (error1 "sexp->int" x))

;; utility functions
(define field=?
  (field:t sa va) (field:t sb vb)
  -> (and (eq? sa sb) (sexp=? va vb)))

;; XXX consider eq? shortcut
(define sexp=?
  (sexp:undef) (sexp:undef)           -> #t
  (sexp:symbol a) (sexp:symbol b)     -> (eq? a b)
  (sexp:bool a) (sexp:bool b)         -> (eq? a b)
  (sexp:int a) (sexp:int b)           -> (= a b)
  (sexp:string a) (sexp:string b)     -> (string=? a b)
  (sexp:char a) (sexp:char b)         -> (char=? a b)
  (sexp:list l0) (sexp:list l1)       -> (every2? sexp=? l0 l1)
  (sexp:vector a) (sexp:vector b)     -> (every2? sexp=? a b)
  (sexp:record a) (sexp:record b)     -> (every2? field=? a b)
  (sexp:cons a0 a1) (sexp:cons b0 b1) -> (and (eq? a0 b0) (eq? a1 b1))
  (sexp:attr a0 a1) (sexp:attr b0 b1) -> (and (sexp=? a0 b0) (eq? a1 b1))
  _ _ -> #f
  )

(define (sexp1 sym rest)
  ;; build an s-expression with <sym> at the front followed by <rest>
  (sexp:list (list:cons (sexp:symbol sym) rest)))

(define repr-field
  (field:t '... _)   -> "..."
  (field:t name val) -> (format (sym name) "=" (p repr val)))

(define repr
  (sexp:list ((sexp:symbol 'quote) x)) -> (format "'" (repr x))
  (sexp:list l)     -> (format "(" (join repr " " l) ")")
  (sexp:symbol s)   -> (format (sym s))
  (sexp:string s)   -> (format "\"" s "\"") ;; XXX escape backslashes...
  (sexp:char ch)    -> (format "#\\" (char ch))
  (sexp:bool #t)    -> "#t"
  (sexp:bool #f)    -> "#f"
  (sexp:int n)      -> (format (int n))
  (sexp:undef)      -> "#u"
  (sexp:vector v)   -> (format "#(" (join repr " " v) ")")
  (sexp:record fl)  -> (format "{" (join repr-field " " fl) "}")
  (sexp:cons dt c)  -> (format (if (eq? dt 'nil) "" (symbol->string dt)) ":" (sym c))
  (sexp:attr lhs a) -> (format (p repr lhs) "." (sym a))
  )

(define indent
  0 -> #t
  n -> (begin (print-string "  ") (indent (- n 1))))

(define pp-size-field
  (field:t name val) -> (+ (+ 1 (string-length (symbol->string name)))
			   (pp-size val)))
(define pp-size
  (sexp:list l)     -> (foldr + (+ 1 (length l)) (map pp-size l))
  (sexp:symbol s)   -> (string-length (symbol->string s))
  (sexp:string s)   -> (+ 2 (string-length s)) ;; escaped backslashes!
  (sexp:char ch)    -> (string-length (repr (sexp:char ch)))
  (sexp:bool _)     -> 2
  (sexp:int n)      -> (string-length (int->string n))
  (sexp:undef)      -> 2
  (sexp:vector v)   -> (foldr + (+ 2 (length v)) (map pp-size v))
  (sexp:record fl)  -> (foldr + (+ (length fl) 1) (map pp-size-field fl))
  (sexp:cons dt c)  -> (+ 1 (+ (string-length (symbol->string dt)) (string-length (symbol->string c))))
  (sexp:attr lhs a) -> (+ 1 (+ (pp-size lhs) (string-length (symbol->string a))))
  )

(define (pp d exp)
  (let ((size (pp-size exp)))
    (if (< size 80)
	(print-string (repr exp))
	(match exp with
	  (sexp:list ())	-> (print-string "()")
	  (sexp:list (hd . tl)) -> (begin (print-string "(")
					  (pp d hd)
					  (for-each
					   (lambda (x)
					     (newline)
					     (indent (+ d 1))
					     (pp (+ d 1) x)) tl)
					  (print-string ")"))
	  ;; XXX complete for vector & record.
	  _ -> (print-string (repr exp))))))

;; (define (test-file)
;;   (let ((t (read-file
;; 	    (if (> sys.argc 1)
;; 		sys.argv[1]
;; 		"lib/core.scm"))))
;;     ;;  (printn t)
;;     ;;(for-each (lambda (x) (printn x) (pp 0 x) (newline)) t)
;;     (printn t)
;;     (for-each (lambda (x) (pp 0 x) (newline)) t)
;;     #u
;;     ))

;(test-file)
