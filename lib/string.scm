;; -*- Mode: Irken -*-

(define (string-tuple-length n)
  (%backend (c llvm)
    (%%cexp (int -> int) "string_tuple_length (%0)" n))
  (%backend bytecode
    (how-many (+ n 4) (get-word-size))))

(define (make-string n)
  (%backend (c llvm)
    (%ensure-heap #f (string-tuple-length n))
    (%%cexp
     (int -> string)
     "(t=alloc_no_clear (TC_STRING, string_tuple_length (%0)), ((pxll_string*)(t))->len = %0, t)"
     n))
  (%backend bytecode
    (%ensure-heap #f (string-tuple-length n))
    (%%cexp (int -> string) "smake" n))
  )

(define (buffer-copy src src-start n dst dst-start)
  (%backend (c llvm)
    ;; XXX range check
    (%%cexp
     (string int string int int -> undefined)
     "memcpy (%0+%1, %2+%3, %4)" dst dst-start src src-start n))
  (%backend bytecode
    (%%cexp (string int int string int -> undefined)
            "scopy"
            src src-start n dst dst-start)))

;; XXX why not use substring?
(define (copy-string s1 n)
  (let ((s2 (make-string n)))
    (buffer-copy s1 0 n s2 0)
    s2))

(define (substring src start end)
  ;; XXX range check
  (let ((n (- end start))
	(r (make-string n)))
    (buffer-copy src start n r 0)
    r))

(define (ascii->char n)
  (%backend (c llvm)
    (%%cexp (int -> char) "TO_CHAR(%0)" n))
  (%backend bytecode
    (%%cexp (int int -> char) "makei" #x02 n))
  )

(define (char->ascii c)
  (%backend (c llvm)
    (%%cexp (char -> int) "GET_CHAR(%0)" c))
  (%backend bytecode
    (%%cexp (char -> int) "unchar" c))
  )

(define (char->string ch)
  (let ((r (make-string 1)))
    (string-set! r 0 ch)
    r))

(define (bool->string b)
  ;; XXX wait, why am I copying this?
  (copy-string (if b "#t" "#f") 2))

(define (string-ref s n)
  (%backend (c llvm)
    (%%cexp ((raw string) int -> undefined) "range_check (((pxll_string *)(%0))->len, %1)" s n)
    (%%cexp (string int -> char) "TO_CHAR(((unsigned char *)%0)[%1])" s n))
  (%backend bytecode
    (%%cexp (string int -> char) "sref" s n)))

(define (string-set! s n c)
  (%backend (c llvm)
    (%%cexp ((raw string) int -> undefined) "range_check (((pxll_string *)(%0))->len, %1)" s n)
    (%%cexp (string int char -> undefined) "%0[%1] = GET_CHAR (%2)" s n c))
  (%backend bytecode
    (%%cexp (string int char -> undefined) "sset" s n c)))

(define (string-concat l)
  ;; merge a list of strings into one string
  (let ((tsize (fold (lambda (s acc) (+ (string-length s) acc)) 0 l))
	(buffer (make-string tsize)))
    (let loop ((l0 l) (pos 0))
      (match l0 with
         () -> buffer
	 (hd . tl) -> (begin
			(buffer-copy hd 0 (string-length hd) buffer pos)
			(loop tl (+ pos (string-length hd))))))))

(defmacro string-append
  (string-append)           -> ""
  (string-append s0)        -> s0
  (string-append s0 s1 ...) -> (string-concat (LIST s0 s1 ...))
  )

(define (string-join l sep)
  (define sj
    ()        acc -> (string-concat (reverse acc))
    (one)     acc -> (string-concat (reverse (list:cons one acc)))
    (hd . tl) acc -> (sj tl (list:cons sep (list:cons hd acc))))
  (if (= (string-length sep) 0)
      (string-concat l)
      (sj l '())))

(define (string-split s ch)
  (let loop ((i 0)
	     (j 0)
	     (acc '()))
    (cond ((= i (string-length s)) (reverse (list:cons (substring s j i) acc)))
	  ((char=? (string-ref s i) ch)
	   (loop (+ i 1) (+ i 1) (list:cons (substring s j i) acc)))
	  (else
	   (loop (+ i 1) j acc)))))

(define (string-compare a b) : (string string -> cmp)
  (%backend (c llvm)
    (let ((alen (string-length a))
          (blen (string-length b))
          (cmp (%%cexp (string string int -> int) "memcmp (%0, %1, %2)" a b (min alen blen))))
      (cond ((= cmp 0)
             (int-cmp alen blen))
            (else (int-cmp cmp 0)))))
  (%backend bytecode
    (magic-cmp a b))
  )

(define (string-find a b)
  ;; find <a> in <b>
  (let ((alen (string-length a))
	(blen (string-length b)))
    (if (< blen alen)
	-1
	(let loop ((i 0) (j 0))
	  (cond ((= j alen) (- i j))
		((= i blen) -1)
		((eq? (string-ref a j) (string-ref b i))
		 (loop (+ i 1) (+ j 1)))
		(else
		 (loop (+ i 1) 0)))))))

(define (starts-with a b)
  ;; does <a> start with <b>?
  (let ((alen (string-length a))
	(blen (string-length b)))
    (if (< alen blen)
	#f
	(let loop ((i 0))
	  (cond ((= i blen) #t)
		((eq? (string-ref a i) (string-ref b i))
		 (loop (+ i 1)))
		(else #f))))))

(define (string=? s1 s2)
  (eq? (string-compare s1 s2) (cmp:=)))
(define (string<? s1 s2)
  (eq? (string-compare s1 s2) (cmp:<)))
(define (string>? s1 s2)
  (eq? (string-compare s1 s2) (cmp:>)))

(define (zero-terminate s)
  (if (char=? (string-ref s (- (string-length s) 1)) #\nul)
      s
      (let ((n (string-length s))
	    (s2 (make-string (+ n 1))))
	(buffer-copy s 0 n s2 0)
	(string-set! s2 n #\nul)
	s2)))

(define (list->string l)
  (let ((buffer (make-string (length l))))
    (let loop ((l l) (i 0))
      (match l with
	() -> buffer
	(hd . tl) -> (begin (string-set! buffer i hd) (loop tl (+ i 1))))
      )))

(define (string->list s)
  (let loop ((l (list:nil)) (n (string-length s)))
    (if (= n 0)
	l
	(loop (list:cons (string-ref s (- n 1)) l) (- n 1)))))

(define (char-class char-list)
  (let ((v (make-vector 256 #f)))

    (define (in-class? ch)
      v[(char->ascii ch)])

    (let loop ((l char-list))
      (match l with
	()        -> in-class?
	(hd . tl) -> (begin (set! v[(char->ascii hd)] #t) (loop tl))
	))))

(define whitespace    '(#\space #\tab #\newline #\return))
(define delimiters     (string->list "()[]{}:"))
(define letters        (string->list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(define all-delimiters (append whitespace delimiters))
(define digits         (string->list "0123456789"))
(define printable      (append digits letters (string->list "!\"#$%&'*+,-./:;<=>?@\\^_`[({|})]~")))

(define whitespace?    (char-class '(#\space #\tab #\newline #\return)))
(define delim?         (char-class all-delimiters))
(define digit?         (char-class digits))
(define letter?        (char-class letters))
(define field?         (char-class (cons #\- (append letters digits))))
(define printable?     (char-class printable))

(define safe-char
  #\space   -> " "
  #\newline -> "\\n"
  #\tab     -> "\\t"
  #\return  -> "\\r"
  #\"       -> "\\\""
  ch        -> (if (printable? ch)
                   (char->string ch)
                   (format "\\x" (zpad 2 (hex (char->ascii ch)))))
  )


(define (repr-string s)
  (format "\"" (join (map safe-char (string->list s))) "\""))

;; XXX should *not* use ascii conversions.
;; really dumb temp version, only works with [0-9]+ !!
(define (string->int s)
  (let ((sl (string-length s)))
    (let loop ((i 0) (n 0))
      (if (= i sl)
	  n
	  (loop (+ i 1)
		(+ (* 10 n)
		   (- (char->ascii (string-ref s i)) 48)))))))

;; XXX pre-compute 0..100

(define (int->string n)
  (if (= 0 n)
      (char->string #\0) ;; don't use a constant here, mutable string
      (let loop ((x (abs n)) (r '()))
	(if (= 0 x)
	    (list->string
	     (if (< n 0) (list:cons #\- r) r))
	    (loop (/ x 10)
		  (list:cons (ascii->char (+ 48 (remainder x 10))) r)
		  )))))

(define hex-table (literal #(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\a #\b #\c #\d #\e #\f)))

(define (int->hex-string n)
  (let loop ((x (abs n)) (r '()))
    (if (= 0 x)
	(list->string
	 (if (< n 0)
	     (list:cons #\- r) r))
	(loop (>> x 4)
	      (list:cons hex-table[(logand x 15)] r)))))

(define (int->oct-string n)
  (let loop ((x (abs n)) (r '()))
    (if (= 0 x)
	(list->string
	 (if (< n 0)
	     (list:cons #\- r) r))
	(loop (>> x 3)
	      (list:cons hex-table[(logand x 7)] r)))))

(define (pad width s left? ch)
  (let ((n (string-length s)))
    (if (> n width)
	s ;; too wide
	(let ((np (- width n)))
	  (if left?
	      (format (list->string (n-of np ch)) s)
	      (format s (list->string (n-of np ch))))))))
(define (lpad w s ch) (pad w s #t ch))
(define (rpad w s ch) (pad w s #f ch))
(define (cpad w s ch)
  (let ((sl (string-length s))
	(lp (+ sl (/ (- w sl) 2))))
    (rpad w (lpad lp s ch) ch)))

(defmacro fitem
  (fitem (<int> n))		-> (int->string n)
  (fitem (<char> ch))		-> (char->string ch)
  (fitem (<bool> b))		-> (bool->string b)
  (fitem (<hex> n))		-> (int->hex-string n)
  (fitem (<oct> n))		-> (int->oct-string n)
  (fitem (<sym> s))		-> (symbol->string s)
  (fitem (<join> l))		-> (string-concat l)
  (fitem (<join> sep l))	-> (string-join l sep)         ;; separate each string in <l> with <sep>
  (fitem (<join> p sep l))	-> (string-join (map p l) sep) ;; map <p> over list <l>, separate each with <sep>
  (fitem (<string> s))          -> (repr-string s)
  (fitem (<lpad> n item ...))	-> (lpad n (format item ...) #\space) ;; left-pad
  (fitem (<rpad> n item ...))	-> (rpad n (format item ...) #\space) ;; right-pad
  (fitem (<cpad> n item ...))	-> (cpad n (format item ...) #\space) ;; center-pad
  (fitem (<zpad> n item ...))	-> (lpad n (format item ...) #\0)     ;; zero-pad
  (fitem (<repeat> n item ...)) -> (string-concat (n-of n (format item ...)))
  (fitem x)			-> x	;; anything else must already be a string
  )

(defmacro formatl
  (formatl) -> (list:nil)
  (formatl item items ...) -> (list:cons (fitem item) (formatl items ...))
  )

(defmacro format
  (format item)	    -> (fitem item)
  (format item ...) -> (string-concat (formatl item ...))
  )

(defmacro format-join
  (format-join sep item ...) -> (string-join (formatl item ...) sep)
  )

(defmacro printf
  (printf item ...) -> (print-string (format item ...))
  )

(define (strlen s)
  (%%cexp (cstring -> int) "strlen(%0)" s))

(define (copy-cstring s)
  (let ((len (strlen s))
	(result (make-string len)))
    (%%cexp (string cstring int -> undefined)
	    "memcpy (%0, %1, %2)"
	    result s len)
    result))

;; read from a string one char at a time...
;; XXX think about generator interfaces...
(define (string-reader s)
  (let ((pos 0)
	(slen (string-length s)))
    (lambda ()
      (if (>= pos slen)
	  #\eof
	  (let ((r (string-ref s pos)))
	    (set! pos (+ 1 pos))
	    r)))))
