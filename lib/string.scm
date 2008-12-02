
;; (define (make-string n)
;;   (%%verify "TC_INT" 1 n)
;;   (let ((s (%make-tuple #x10 (%%cexp "box(string_tuple_length(unbox(%s)))" n))))
;;     (%%cexp "((pxll_string*)(%s))->len = unbox(%s)" s n)
;;     s))

(define (make-string n)
  (%%verify "TC_INT" 1 n)
  (%%cexp
   "( t = %s, ((pxll_string*)(t))->len = unbox(%s), t)"
   (%make-tuple #x10 (%%cexp "box(string_tuple_length(unbox(%s)))" n))
   n))

(define (copy-string s1 n)
  (%%verify "TC_STRING" 1 s1)
  (%%verify "TC_INT" 1 n)
  (let ((s2 (make-string n)))
    (%%cexp "(memcpy (GET_STRING_POINTER(%s), GET_STRING_POINTER(%s), unbox(%s)), PXLL_UNDEFINED)" s2 s1 n)
    s2))

(define (buffer-copy src src-start n dst dst-start)
  (%%verify "TC_STRING" 1 src)
  (%%verify "TC_STRING" 1 dst)
  (%%verify "TC_INT" 1 n)
  (%%verify "TC_INT" 1 src-start)
  (%%verify "TC_INT" 1 dst-start)
  (%%cexp "(memcpy (GET_STRING_POINTER(%s)+unbox(%s), GET_STRING_POINTER(%s)+unbox(%s), unbox(%s)))" dst dst-start src src-start n))

(define (ascii->char n)
  (%%verify "TC_INT" 1 n)
  (%make-immediate n #x02))

(define (char->ascii c)
  (%%verify "TC_CHAR" 1 c)
  (%%cexp "box (GET_PAYLOAD(%s))" c))

(define (string-length s)
  (%%verify "TC_STRING" 1 s)
  (%%cexp "box(((pxll_string *)(%s))->len)" s))

;; XXX need range-check
(define (string-ref s n)
  (%%verify "TC_STRING" 1 s)
  (%%verify "TC_INT" 1 n)
  (%%cexp "TO_CHAR(((pxll_string *)%s)->data[unbox(%s)])" s n)
  )

;; XXX need range-check
(define (string-set! s n c)
  (%%verify "TC_STRING" 1 s)
  (%%verify "TC_INT" 1 n)
  (%%verify "TC_CHAR" 1 c)
  (%%cexp "(((pxll_string *)%s)->data[unbox(%s)] = GET_PAYLOAD (%s), PXLL_UNDEFINED)" s n c)
  )

(define (substring str start end)
  ;; XXX range-check
  (let ((r (make-string (- end start))))
    (buffer-copy str start (- end start) r 0)
    r))

(define (print-string s)
  (%%verify "TC_STRING" 1 s)
  (%%cexp "box (fwrite (GET_STRING_POINTER(%s), 1, unbox(%s), stdout))" s (string-length s)))

(define (print-int n)
  (%%verify "TC_INT" 1 n)
  (%%cexp "box (fprintf (stdout, \"%%d\", unbox(%s)))" n))

(define (string-compare a b)
  (%%verify "TC_STRING" 1 a)
  (%%verify "TC_STRING" 1 b)
  (let* ((alen (string-length a))
	 (blen (string-length b))
	 (min (if (%lt? alen blen) alen blen))
	 (cmp (%%cexp "box (memcmp (GET_STRING_POINTER(%s), GET_STRING_POINTER(%s), unbox(%s)))" a b min)))
    (cond ((%eq? cmp 0)
	   (if (%eq? alen blen)
	       0
	       (if (%lt? alen blen) -1 1)))
	  (else cmp))))

(define (string-=? s1 s2)
  (= (string-compare s1 s2) 0))
(define (string-<? s1 s2)
  (< (string-compare s1 s2) 0))
(define (string->? s1 s2)
  (> (string-compare s1 s2) 0))

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

;; same thing, rewritten to avoid allocation in the loops
;; [which doesn't work because %%verify expressions cause
;;  more-than-one-reference in small functions which trigger
;;  let* bindings when inlining, thus allocation]
(define (string-join . strings)
  (let ((i 0)
	(size 0)
	(pos 0)
	(istr 0)
	(ilen 0)
	(r #f))
    (let size-loop ()
      (cond ((< i (tuple-length strings))
	     (set! size (+ size (string-length (tuple-ref strings i))))
	     (set! i (+ i 1))
	     (size-loop))
	    (else
	     (set! r (make-string size))
	     (set! i 0)
	     (let copy-loop ()
	       (cond ((< i (tuple-length strings))
		      (set! istr (tuple-ref strings i))
		      (set! ilen (string-length istr))
		      (buffer-copy istr 0 ilen r pos)
		      (set! i (+ i 1))
		      (set! pos (+ pos ilen))
		      (copy-loop))
		     (else
		      r)))
	     )))
    ))

;; need print-char

(define (terpri)
  (print-string "\n"))

(define (list->string l)
  (let ((buffer (make-string (length l))))
    (let loop ((l l) (i 0))
      (if (null? l)
	  buffer
	  (begin
	    (string-set! buffer i (car l))
	    (loop (cdr l) (%+ i 1)))))))

(define (string->list s)
  (let loop ((l '()) (n (string-length s)))
    (if (= n 0)
	l
	(loop (cons (string-ref s (- n 1)) l) (- n 1)))))

(define (sys.argc)
  (%%cexp "box(argc)"))

(define (sys.argv n)
  (let* ((len (%%cexp "box(strlen(argv[unbox(%s)]))" n))
	 (r (make-string len)))
    (%%cexp "(memcpy (GET_STRING_POINTER(%s), argv[unbox(%s)], unbox(%s)), PXLL_UNDEFINED)" r n len)
    r))
