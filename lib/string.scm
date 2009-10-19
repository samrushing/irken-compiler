
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
  (%%cexp (string/raw -> int) "%s->len" s))

(define (string-ref s n)
  ;; XXX need range-check
  (%%cexp (string int -> char) "TO_CHAR(%s[%s])" s n)
  )

(define (string-set! s n c)
  ;; XXX need range-check
  (%%cexp
   (string int char -> undefined)
   "(%s[%s] = GET_CHAR (%s), PXLL_UNDEFINED)" s n c)
  )

(define (string-compare a b)
  ;; it'd be nice if the compiler could get rid of this let*,
  ;;   it sucks to have to allocate in order to compare two strings.
  (let* ((alen (string-length a))
	 (blen (string-length b))
	 (min (if (< alen blen) alen blen))
	 (cmp (%%cexp (string string int -> int) "memcmp (%s, %s, %s)" a b min)))
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

;; [waiting for type inference to support nary args]
;; (define (string-join . strings)
;;   (let size-loop ((i 0)
;;                   (size 0))
;;     (if (< i (tuple-length strings))
;;         (size-loop (+ i 1) (+ size (string-length (tuple-ref strings i))))
;;         (let ((r (make-string size)))
;;           (let copy-loop ((j 0)
;;                           (pos 0))
;;             (if (< j (tuple-length strings))
;;                 (let* ((jstr (tuple-ref strings j))
;;                        (jlen (string-length jstr)))
;;                   (buffer-copy jstr 0 jlen r pos)
;;                   (copy-loop (+ j 1) (+ pos jlen)))
;;                 r))))))

;; [waiting to decide how I will deal with lists]

;; (define (list->string l)
;;   (let ((buffer (make-string (length l))))
;;     (let loop ((l l) (i 0))
;;       (if (null? l)
;; 	  buffer
;; 	  (begin
;; 	    (string-set! buffer i (car l))
;; 	    (loop (cdr l) (+ i 1)))))))

;; (define (string->list s)
;;   (let loop ((l '()) (n (string-length s)))
;;     (if (= n 0)
;; 	l
;; 	(loop (cons (string-ref s (- n 1)) l) (- n 1)))))

(define (sys:argc)
  (%%cexp (-> int) "argc"))

(define (sys:argv n)
  (let* ((len (%%cexp (int -> int) "strlen(argv[%s])" n))
	 (r (make-string len)))
    (%%cexp (string int int -> undefined)
	    "(memcpy (%s, argv[%s], %s), PXLL_UNDEFINED)" r n len)
    r))
