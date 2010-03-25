
(define (make-string n)
  (%%cexp
   (int int -> string)
   "(t=alloc_no_clear (TC_STRING, string_tuple_length (%s)), ((pxll_string*)(t))->len = %s, t)"
   n
   n))

(define (copy-string s1 n)
  (let ((s2 (make-string n)))
    (%%cexp (string string int -> undefined) "(memcpy (%s, %s, %s), PXLL_UNDEFINED)" s2 s1 n)
    s2))

(define (buffer-copy src src-start n dst dst-start)
  (%%cexp
   (string int string int int -> undefined)
   "(memcpy (%s+%s, %s+%s, %s), PXLL_UNDEFINED)" dst dst-start src src-start n))

(define (substring src start end)
  ;; XXX range check
  (let* ((n (- end start))
	 (r (make-string n)))
    (buffer-copy src start n r 0)
    r))

(define (ascii->char n)
  (%%cexp (int -> char) "TO_CHAR(%s)" n))

(define (char->ascii c)
  (%%cexp (char -> int) "GET_CHAR(%s)" c))

(define (string-length s)
  (%%cexp ((raw string) -> int) "%s->len" s))

(define (string-ref s n)
  ;; XXX need range-check
  (%%cexp (string int -> char) "TO_CHAR(((unsigned char *)%s)[%s])" s n))

(define (string-set! s n c)
  ;; XXX need range-check
  (%%cexp
   (string int char -> undefined)
   "(%s[%s] = GET_CHAR (%s), PXLL_UNDEFINED)" s n c))

(define (string-compare a b)
  (let* ((min (min (string-length a) (string-length b)))
	 (cmp (%%cexp (string string int -> int) "memcmp (%s, %s, %s)" a b min)))
    (cond ((= cmp 0)
	   (if (= (string-length a) (string-length b))
	       0
	       (if (< (string-length a) (string-length b)) -1 1)))
	  (else cmp))))

(define (string-compare a b)
  (let* ((alen (string-length a))
	 (blen (string-length b))
	 (cmp (%%cexp (string string int -> int) "memcmp (%s, %s, %s)" a b (min alen blen))))
    (cond ((= cmp 0)
	   (if (= alen blen)
	       0
	       (if (< alen blen) -1 1)))
	  (else cmp))))

(define (string-=? s1 s2)
  (= (string-compare s1 s2) 0))
(define (string-<? s1 s2)
  (< (string-compare s1 s2) 0))
(define (string->? s1 s2)
  (> (string-compare s1 s2) 0))

(define (zero-terminate s)
  (if (char=? (string-ref s (- (string-length s) 1)) #\null)
      s
      (let ((n (string-length s))
	    (s2 (make-string (+ n 1))))
	(buffer-copy s 0 n s2 0)
	s2)))

(define (list->string l)
  (let ((buffer (make-string (length l))))
    (let loop ((l l) (i 0))
      (vcase list l
	 ((:nil) buffer)
	 ((:cons hd tl)
	  (string-set! buffer i hd)
	  (loop tl (+ i 1)))))))

(define (string->list s)
  (let loop ((l (list:nil)) (n (string-length s)))
    (if (= n 0)
	l
	(loop (list:cons (string-ref s (- n 1)) l) (- n 1)))))

(define (sys:argc)
  (%%cexp (-> int) "argc"))

(define sys:argv

  (let ((nargs (sys:argc))
	(v (%make-vector nargs "")))

    (define (argv n)
      (let* ((len (%%cexp (int -> int) "strlen(argv[%s])" n))
	     (r (make-string len)))
	(%%cexp (string int int -> undefined) "(memcpy (%s, argv[%s], %s), PXLL_UNDEFINED)" r n len)
	r))

    (let loop ((n (sys:argc)))
      (cond ((zero? n) v)
	    (else
	     (set! v[(- n 1)] (argv (- n 1)))
	     (loop (- n 1)))))))
