;; -*- Mode: Irken -*-

(define (string-tuple-length n)
  (%backend c
    (%%cexp (int -> int) "string_tuple_length (%0)" n))
  (%backend (bytecode llvm)
    (how-many (+ n 4) (get-word-size)))
  )

(define (make-string n)
  (%ensure-heap #f (string-tuple-length n))
  (%backend c
    (%%cexp
     (int -> string)
     "(t=alloc_no_clear (TC_STRING, string_tuple_length (%0)), ((irk_string*)(t))->len = %0, t)"
     n))
  (%backend llvm
    (%llvm-call ("@irk_make_string" (int -> string) ccc) n))
  (%backend bytecode
    (%%cexp (int -> string) "smake" n))
  )

(defmacro string-range-check
  (string-range-check s n)
  -> (when (not (and (<= 0 n) (< n (string-length s))))
       (raise (:String/Range s n n)))
  (string-range-check s lo hi)
  -> (when (not (and (<= 0 lo) (<= lo hi) (<= hi (string-length s))))
       (raise (:String/Range s lo hi))))

(define (buffer-copy src src-start n dst dst-start)
  (string-range-check src src-start (+ src-start n))
  (string-range-check dst dst-start (+ dst-start n))
  (%backend c
    (%%cexp
     (string int string int int -> undefined)
     "memcpy (%0+%1, %2+%3, %4)" dst dst-start src src-start n))
  (%backend llvm
    (%llvm-call
     ("@irk_buffer_copy" (string int int string int -> undefined))
     src src-start n dst dst-start))
  (%backend bytecode
    (%%cexp
     (string int int string int -> undefined)
     "scopy"
     src src-start n dst dst-start)))

;; XXX need a %copy-tuple prim. (or %copy-object?)
;; XXX why not use substring?
(define (copy-string s1 n)
  (let ((s2 (make-string n)))
    (buffer-copy s1 0 n s2 0)
    s2))

(define (substring src start end)
  (let ((n (- end start))
	(r (make-string n)))
    (buffer-copy src start n r 0)
    r))

(define (char->string ch)
  (let ((r (make-string 1)))
    (string-set! r 0 ch)
    r))

(define (bool->string b)
  ;; we copy because strings are mutable.
  (copy-string (if b "#t" "#f") 2))

(define (string-ref s n)
  (string-range-check s n)
  (%backend c
    (%%cexp (string int -> char) "TO_CHAR(((unsigned char *)%0)[%1])" s n))
  (%backend llvm
    (%llvm-call ("@irk_string_ref" (string int -> char)) s n))
  (%backend bytecode
    (%%cexp (string int -> char) "sref" s n)))

(define (string-set! s n c)
  (string-range-check s n)
  (%backend c
    (%%cexp (string int char -> undefined) "%0[%1] = GET_CHAR (%2)" s n c)
    #u) ;; avoid C warning.
  (%backend llvm
    (%llvm-call ("@irk_string_set" (string int char -> undefined)) s n c))
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
  (string-append s0 s1 ...) -> (string-concat (list s0 s1 ...))
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

;; XXX this needs to be renamed 'string-cmp'
(define (string-compare a b) : (string string -> cmp)
  (%backend c
    (%%cexp ((raw string) (raw string) -> cmp) "irk_string_cmp (%0, %1)" a b))
  (%backend llvm
    (%llvm-call ("@irk_string_cmp" (string string -> cmp) ccc) a b))
  (%backend bytecode
    (magic-cmp a b)))

;; alias
(define string-cmp string-compare)

(define (string-find-from a b pos)
  ;; find <a> in <b>, starting at b[pos]
  (let ((alen (string-length a))
	(blen (string-length b)))
    (if (< blen alen)
	-1
	(let loop ((i pos) (j 0))
	  (cond ((= j alen) (- i j))
		((= i blen) -1)
		((eq? (string-ref a j) (string-ref b i))
		 (loop (+ i 1) (+ j 1)))
		(else
		 (loop (+ i 1) 0)))))))

(define (string-find a b)
  (string-find-from a b 0))

(define (string-split-string src split)
  (let ((len0 (string-length src))
        (len1 (string-length split))
        (r '()))
    (when (> len1 len0)
      (raise (:String/SplitTooLong split)))
    (let loop ((pos 0))
      (let ((where (string-find-from split src pos)))
        (if (= where -1)
            (begin
              (push! r (substring src pos len0))
              (reverse r))
            (begin
              (push! r (substring src pos where))
              (loop (+ where len1))))))))

(define (string-replace-all src from to)
  (let ((parts (string-split-string src from)))
    (format (join to parts))))

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

(define (ends-with a b)
  ;; does <a> end with <b>?
  (let ((alen (string-length a))
	(blen (string-length b)))
    (if (< alen blen)
	#f
	(let loop ((i 0))
	  (cond ((= i blen) #t)
		((eq? (string-ref a (- alen 1 i)) (string-ref b (- blen 1 i)))
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
  (let ((len (length l))
        (buffer (make-string len))
        (i 0))
    (for-list ch l
      (string-set! buffer i ch)
      (set! i (+ i 1)))
    buffer))

(define (string->list s)
  (let loop ((l (list:nil)) (n (string-length s)))
    (if (= n 0)
	l
	(loop (list:cons (string-ref s (- n 1)) l)
              (- n 1)))))

(define (char-class char-list)
  (let ((v (make-vector 256 #f)))
    (define (in-class? ch) v[(char->ascii ch)])
    (for-list ch char-list
      (set! v[(char->ascii ch)] #t))
    in-class?
    ))

(define whitespace    '(#\space #\tab #\newline #\return))
(define delimiters     (string->list "()[]{}:;"))
(define lowercase      (string->list "abcdefghijklmnopqrstuvwxyz"))
(define uppercase      (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(define letters        (append lowercase uppercase))
(define all-delimiters (append whitespace delimiters))
(define digits         (string->list "0123456789"))
(define hex-digits     (string->list "0123456789ABCDEFabcdef"))
(define printable      (append digits letters (string->list "!\"#$%&'*+,-./:;<=>?@\\^_`[({|})]~")))

(define whitespace?    (char-class whitespace))
(define delim?         (char-class all-delimiters))
(define digit?         (char-class digits))
(define hex-digit?     (char-class hex-digits))
(define lower?         (char-class lowercase))
(define upper?         (char-class uppercase))
(define letter?        (char-class letters))
(define field?         (char-class (cons #\- (append letters digits))))
(define printable?     (char-class printable))

;; ascii only
(define (toupper ch)
  (if (lower? ch)
      (ascii->char (- (char->ascii ch) 32))
      ch))

(define (upcase s)
  (list->string (map toupper (string->list s))))

(define (tolower ch)
  (if (upper? ch)
      (ascii->char (+ (char->ascii ch) 32))
      ch))

(define (downcase s)
  (list->string (map tolower (string->list s))))

(define safe-char
  #\space   -> " "
  #\newline -> "\\n"
  #\tab     -> "\\t"
  #\return  -> "\\r"
  #\"       -> "\\\""
  #\\       -> "\\\\"
  ch        -> (if (printable? ch)
                   (char->string ch)
                   (format "\\x" (zpad 2 (hex (char->ascii ch)))))
  )

;; XXX this should be called string-repr!
(define (repr-string s)
  (format "\"" (join (map safe-char (string->list s))) "\""))

(define (string->int s)
  (let ((digits (string->list s)))
    (define (d2i ch)
      (if (digit? ch)
          (- (char->int ch) 48)
          (raise (:String/BadInt s))))
    (define ds2i
      acc ()        -> acc
      acc (hd . tl) -> (ds2i (+ (* 10 acc) (d2i hd)) tl))
    (match digits with
      (#\- . rest) -> (- (ds2i 0 rest))
      _            -> (ds2i 0 digits)
      )))

;; pre-compute 0..100?
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

(%backend (c llvm)

  ;; these are now somewhat obsolete because FFI.
  (define (strlen s)
    (%%cexp (cstring -> int) "strlen(%0)" s))

  (define (copy-cstring s)
    (let ((len (strlen s))
          (result (make-string len)))
      (%%cexp (string cstring int -> undefined)
              "memcpy (%0, %1, %2)"
              result s len)
      result))
  )

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

(defmacro for-string
  (for-string chname s body ...)
  -> (let (($s s)) ;; avoid duplicating <s> expression.
       (for-range $i (string-length $s)
	 (let ((chname (string-ref $s $i)))
	   body ...))))

(define (string-generator s)
  (makegen emit
    (for-string ch s
      (emit ch))))


