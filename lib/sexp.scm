;; -*- Mode: Irken -*-

;; an s-expression datatype.

;; every arm carries the source location of its first character (or
;;  (src:nil) for one made by a program rather than read).  equality and
;;  printing ignore it.

(datatype sexp
  (:list (list sexp) src)
  (:symbol symbol src)
  (:string string src)
  (:char char src)
  (:bool bool src)
  (:int int src)
  (:undef src)
  (:vector (list sexp) src)
  (:record (list field) src)
  (:cons symbol symbol src) ;; constructor ':' syntax
  (:attr sexp symbol src)	;; attribute '.' syntax
  )

(datatype field
  (:t symbol sexp src)  ;; name=value, src at the name
  )

;; metadata about the source location of an s-expression.

(datatype src
  (:nil)                                 ;; no src information
  (:base {path=string line=int pos=int}) ;; original src location
  (:inline symbol src src)               ;; inlined function (second `src` is closer to the origin)
  (:macro symbol src src)                ;; macro expansion (ditto)
  )

;; a location chain, flattened innermost first: each base location, and
;;  between them, what happened there.  for example, an error in the
;;  body of `binary+`, inlined into a call made by the `+` macro, used
;;  at `(+ 1 "x")` in the user's code, is
;;    core.scm:84 (inlined binary+) core.scm:103 (macro +) user.scm:2
(datatype src-step
  (:at {path=string line=int pos=int})
  (:macro symbol)    ;; the location after this one is where the macro was used
  (:inlined symbol)  ;; ... where the function was inlined
  )

(define (src-flatten src)
  (define F
    (src:nil)             acc -> acc
    (src:base r)          acc -> (list:cons (src-step:at r) acc)
    (src:macro m site s)  acc -> (F site (list:cons (src-step:macro m) (F s acc)))
    (src:inline f s site) acc -> (F site (list:cons (src-step:inlined f) (F s acc)))
    )
  (reverse (F src '())))

;; inlining renames functions: `binary+_177` -> `binary+`
(define (src-name sym)
  (let ((s (symbol->string sym))
        (n (string-length s)))
    (let loop ((i 0))
      (cond ((>= (+ i 1) n) s)
            ((and (eq? (string-ref s i) #\_) (digit? (string-ref s (+ i 1))))
             (substring s 0 i))
            (else (loop (+ i 1)))))))

(define (src-loc-repr r)
  (format r.path ":" (int r.line) ":" (int r.pos)))

;; the note for a location: what happened there.
(define src-step-note
  (src-step:macro m)   -> (format "macro `" (src-name m) "` used here")
  (src-step:inlined f) -> (format "`" (src-name f) "` inlined here")
  _                    -> ""
  )

;; one line per location, innermost first, in GNU `file:line:col:` form.
(define (src-repr src limit)
  (define L
    acc note ()
    -> (reverse acc)
    acc note ((src-step:at r) . tl)
    -> (L (list:cons (format (src-loc-repr r) ": " note) acc) "" tl)
    acc note (step . tl)
    -> (L acc (src-step-note step) tl)
    )
  (let ((lines (L '() "" (src-flatten src))))
    (cond ((null? lines) "<no source location>\n")
          ((> (length lines) limit)
           (format (join "\n" (take lines limit)) "\n...\n"))
          (else (format (join "\n" lines) "\n")))))

(define null-src (src:nil))

;; the first source location found in <exp> (in reading order).
(define (guess-location exp)
  (let/cc return

    (define found
      (src:nil) -> #u
      src       -> (return src)
      )

    (define Wf
      (field:t name val src) -> (begin (found src) (W val))
      )

    (define (W x)
      (found (sexp/src x))
      (match x with
        (sexp:list l _)      -> (for-each W l)
        (sexp:vector v _)    -> (for-each W v)
        (sexp:record fl _)   -> (for-each Wf fl)
        (sexp:attr lhs _ _)  -> (W lhs)
        _ -> #u
        ))

    (W exp)
    null-src
    ))

;; ----- sexp format macro ------

;; build a (list field)
(defmacro sexprec
  (sexprec) -> '()
  (sexprec (name val) rest ...)
  -> (list:cons
      (field:t (quote name) (sexpf val) null-src)
      (sexprec rest ...))
  )

;; format one sexp
(defmacro sexpf
  (sexpf (<int> n))         -> (sexp:int n null-src)
  (sexpf (<char> ch))       -> (sexp:char ch null-src)
  (sexpf (<bool> b))        -> (sexp:bool b null-src)
  (sexpf (<sym> s))         -> (sexp:symbol s null-src)
  (sexpf (<symsrc> s src))  -> (sexp:symbol s src)
  (sexpf (<string> s))      -> (sexp:string s null-src)
  (sexpf (<list> exp))      -> (sexp:list exp null-src) ;; exp is (list sexp)
  (sexpf (<rec> field ...)) -> (sexp:record (sexprec field ...) null-src)
  (sexpf (<undef>))         -> (sexp:undef null-src)
  (sexpf (<cons> dt tag))   -> (sexp:cons dt tag null-src)
  (sexpf (<attr> exp name)) -> (sexp:attr (sexpf exp) name null-src)
  (sexpf (<vec> exp ...))   -> (sexp:vector (sexpl exp ...) null-src)
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
  (sexp)         -> (sexp:list '() null-src)
  (sexp exp ...) -> (sexp:list (sexpl exp ...) null-src)
  )

;; ------------------------------

(define sexp->symbol
  (sexp:symbol s _) -> s
  x -> (error1 "sexp->symbol" x))

(define (make-sexp-symbol s)
  (sexp:symbol s null-src))

(define (make-sexp-int n)
  (sexp:int n null-src))

(define (make-sexp-string s)
  (sexp:string s null-src))

;; the source location of any sexp.
(define sexp/src
  (sexp:list _ src)     -> src
  (sexp:symbol _ src)   -> src
  (sexp:string _ src)   -> src
  (sexp:char _ src)     -> src
  (sexp:bool _ src)     -> src
  (sexp:int _ src)      -> src
  (sexp:undef src)      -> src
  (sexp:vector _ src)   -> src
  (sexp:record _ src)   -> src
  (sexp:cons _ _ src)   -> src
  (sexp:attr _ _ src)   -> src
  )

;; the same sexp, at another location (the outermost form only).
(define sexp/at
  src (sexp:list l _)       -> (sexp:list l src)
  src (sexp:symbol s _)     -> (sexp:symbol s src)
  src (sexp:string s _)     -> (sexp:string s src)
  src (sexp:char c _)       -> (sexp:char c src)
  src (sexp:bool b _)       -> (sexp:bool b src)
  src (sexp:int n _)        -> (sexp:int n src)
  src (sexp:undef _)        -> (sexp:undef src)
  src (sexp:vector v _)     -> (sexp:vector v src)
  src (sexp:record fl _)    -> (sexp:record fl src)
  src (sexp:cons dt alt _)  -> (sexp:cons dt alt src)
  src (sexp:attr lhs a _)   -> (sexp:attr lhs a src)
  )

(define sexp->int
  (sexp:int n _) -> n
  x -> (error1 "sexp->int" x))

;; utility functions
(define field=?
  (field:t sa va _) (field:t sb vb _)
  -> (and (eq? sa sb) (sexp=? va vb)))

(define field<?
  (field:t sa va _) (field:t sb vb _)
  -> (symbol<? sa sb))

(define sexp=?
  (sexp:symbol s0 _)   (sexp:symbol s1 _)   -> (eq? s0 s1)
  (sexp:char ch0 _)    (sexp:char ch1 _)    -> (eq? ch0 ch1)
  (sexp:bool b0 _)     (sexp:bool b1 _)     -> (eq? b0 b1)
  (sexp:int n0 _)      (sexp:int n1 _)      -> (= n0 n1)
  (sexp:undef _)       (sexp:undef _)       -> #t
  (sexp:cons dt0 c0 _) (sexp:cons dt1 c1 _) -> (and (eq? dt0 dt1) (eq? c0 c1))
  (sexp:string s0 _)   (sexp:string s1 _)   -> (string=? s0 s1)
  (sexp:vector v0 _)   (sexp:vector v1 _)   -> (every2? sexp=? v0 v1)
  (sexp:record fl0 _)  (sexp:record fl1 _)  -> (every2? field=? fl0 fl1)
  (sexp:attr lhs0 a0 _)  (sexp:attr lhs1 a1 _)  -> (and (eq? a0 a1) (sexp=? lhs0 lhs1))
  (sexp:list l0 _)     (sexp:list l1 _)     -> (every2? sexp=? l0 l1)
  _ _                                       -> #f
  )

(define (sexp1 sym rest)
  ;; build an s-expression with <sym> at the front followed by <rest>
  (sexp:list (list:cons (sexp:symbol sym null-src) rest) null-src))

(define (sexp1src sym src rest)
  ;; build an s-expression with <sym> at the front followed by <rest>
  (sexp:list (list:cons (sexp:symbol sym src) rest) null-src))

(define repr-field
  (field:t '... _ _) -> "..."
  (field:t name val _) -> (format (sym name) "=" (repr val)))

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
  (sexp:list ((sexp:symbol 'quote _) x) _) -> (format "'" (repr x))
  (sexp:list l _)   -> (format "(" (join repr " " l) ")")
  (sexp:symbol s _) -> (format (sym s))
  (sexp:string s _) -> (repr-string s)
  (sexp:char ch _)  -> (repr-char ch)
  (sexp:bool #t _)  -> "#t"
  (sexp:bool #f _)  -> "#f"
  (sexp:int n _)    -> (format (int n))
  (sexp:undef _)    -> "#u"
  (sexp:vector v _) -> (format "#(" (join repr " " v) ")")
  (sexp:record fl _)  -> (format "{" (join repr-field " " fl) "}")
  (sexp:cons dt c _)  -> (format (if (eq? dt 'nil) "" (symbol->string dt)) ":" (sym c))
  (sexp:attr lhs a _) -> (format (repr lhs) "." (sym a))
  )

(define (indent n)
  (printf (repeat n " ")))

(define pp-size-field
  (field:t name val _) -> (+ (+ 1 (string-length (symbol->string name)))
			   (pp-size val)))
(define pp-size
  (sexp:list l _)   -> (foldr binary+ (+ 1 (length l)) (map pp-size l))
  (sexp:symbol s _) -> (string-length (symbol->string s))
  (sexp:string s _) -> (+ 2 (string-length s)) ;; escaped backslashes!
  (sexp:char ch _)  -> (string-length (repr (sexp:char ch null-src)))
  (sexp:bool _ _)   -> 2
  (sexp:int n _)    -> (string-length (int->string n))
  (sexp:undef _)    -> 2
  (sexp:vector v _) -> (foldr binary+ (+ 2 (length v)) (map pp-size v))
  (sexp:record fl _)  -> (foldr binary+ (+ (length fl) 1) (map pp-size-field fl))
  (sexp:cons dt c _)  -> (+ 1 (+ (string-length (symbol->string dt)) (string-length (symbol->string c))))
  (sexp:attr lhs a _) -> (+ 1 (+ (pp-size lhs) (string-length (symbol->string a))))
  )

(define (pp* exp width depth)
  (define (recur d exp)
    (let ((size (pp-size exp)))
      (if (< size width)
	  (printf (repr exp))
	  (match exp with
	    (sexp:list () _)
	    -> (printf "()")
	    (sexp:list (hd . tl) _)
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
  (recur depth exp)
  (print-string "\n")
  )

(define (pp exp width)
  (pp* exp width 0))
