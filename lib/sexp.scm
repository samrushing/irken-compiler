;; -*- Mode: Irken -*-

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

(datatype field
  (:t symbol sexp)
  )

;; ----- sexp format macro ------

;; build a (list field)
(defmacro sexprec
  (sexprec) -> '()
  (sexprec (name val) rest ...)
  -> (list:cons
      (field:t (quote name) (sexpf val))
      (sexprec rest ...))
  )

;; format one sexp
(defmacro sexpf
  (sexpf (<int> n))         -> (sexp:int n)
  (sexpf (<char> ch))       -> (sexp:char ch)
  (sexpf (<bool> b))        -> (sexp:bool b)
  (sexpf (<sym> s))         -> (sexp:symbol s)
  (sexpf (<string> s))      -> (sexp:string s)
  (sexpf (<list> exp))      -> (sexp:list exp) ;; exp is (list sexp)
  (sexpf (<rec> field ...)) -> (sexp:record (sexprec field ...))
  (sexpf (<undef>))         -> (sexp:undef)
  (sexpf (<cons> dt tag))   -> (sexp:cons dt tag)
  (sexpf (<attr> exp name)) -> (sexp:attr (sexpf exp) name)
  (sexpf (<vec> exp ...))   -> (sexp:vector (sexpl exp ...))
  ;; XXX append & splice?
  (sexpf exp)               -> exp ;; already a sexp
  )

;; build a (list sexp)
(defmacro sexpl
  (sexpl)              -> (list:nil)
  (sexpl exp exps ...) -> (list:cons (sexpf exp) (sexpl exps ...))
  )

;; build a sexp:list
(defmacro sexp
  (sexp)         -> (sexp:list '())
  (sexp exp ...) -> (sexp:list (sexpl exp ...))
  )

;; ------------------------------

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

(define field<?
  (field:t sa va) (field:t sb vb)
  -> (symbol<? sa sb))

(define (sexp=? a b)
  (eq? (cmp:=) (magic-cmp a b)))

(define (sexp1 sym rest)
  ;; build an s-expression with <sym> at the front followed by <rest>
  (sexp:list (list:cons (sexp:symbol sym) rest)))

(define repr-field
  (field:t '... _)   -> "..."
  (field:t name val) -> (format (sym name) "=" (repr val)))

(define repr-char
  #\newline -> "#\\newline"
  #\space   -> "#\\space"
  #\return  -> "#\\return"
  #\tab     -> "#\\tab"
  #\eof     -> "#\\eof"
  #\nul     -> "#\\nul"
  ch        -> (format "#\\" (char ch))
  )

(define repr
  (sexp:list ((sexp:symbol 'quote) x)) -> (format "'" (repr x))
  (sexp:list l)     -> (format "(" (join repr " " l) ")")
  (sexp:symbol s)   -> (format (sym s))
  (sexp:string s)   -> (repr-string s)
  (sexp:char ch)    -> (repr-char ch)
  (sexp:bool #t)    -> "#t"
  (sexp:bool #f)    -> "#f"
  (sexp:int n)      -> (format (int n))
  (sexp:undef)      -> "#u"
  (sexp:vector v)   -> (format "#(" (join repr " " v) ")")
  (sexp:record fl)  -> (format "{" (join repr-field " " fl) "}")
  (sexp:cons dt c)  -> (format (if (eq? dt 'nil) "" (symbol->string dt)) ":" (sym c))
  (sexp:attr lhs a) -> (format (repr lhs) "." (sym a))
  )

(define (indent n)
  (printf (repeat n " ")))

(define pp-size-field
  (field:t name val) -> (+ (+ 1 (string-length (symbol->string name)))
			   (pp-size val)))
(define pp-size
  (sexp:list l)     -> (foldr binary+ (+ 1 (length l)) (map pp-size l))
  (sexp:symbol s)   -> (string-length (symbol->string s))
  (sexp:string s)   -> (+ 2 (string-length s)) ;; escaped backslashes!
  (sexp:char ch)    -> (string-length (repr (sexp:char ch)))
  (sexp:bool _)     -> 2
  (sexp:int n)      -> (string-length (int->string n))
  (sexp:undef)      -> 2
  (sexp:vector v)   -> (foldr binary+ (+ 2 (length v)) (map pp-size v))
  (sexp:record fl)  -> (foldr binary+ (+ (length fl) 1) (map pp-size-field fl))
  (sexp:cons dt c)  -> (+ 1 (+ (string-length (symbol->string dt)) (string-length (symbol->string c))))
  (sexp:attr lhs a) -> (+ 1 (+ (pp-size lhs) (string-length (symbol->string a))))
  )

(define (pp exp width)
  (define (recur d exp)
    (let ((size (pp-size exp)))
      (if (< size width)
	  (printf (repr exp))
	  (match exp with
	    (sexp:list ())
	    -> (printf "()")
	    (sexp:list (hd . tl))
	    -> (begin (printf "(")
		      (recur d hd)
		      (for-each
		       (lambda (x)
			 (newline)
			 (indent (+ d 1))
			 (recur (+ d 1) x)) tl)
		      (printf ")"))
	    ;; XXX complete for vector & record.
	    _ -> (printf (repr exp))))))
  (recur 0 exp)
  (print-string "\n")
  )

