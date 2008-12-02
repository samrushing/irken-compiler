
(define (call/cc p)
  (let ((k (%getcc)))
    (p (lambda (r) (%putcc k r)))))

(define (error arg)
  (%printn arg))

(define (open path flags)
  (let ((fd (cexp "box (open (GET_STRING_POINTER(%s), unbox(%s)))" path flags)))
    (if (%ge? fd 0)
	fd
	(error "open() failed"))))

(define (make-string n)
  (let ((s (%make-tuple #x10 (cexp "box(string_tuple_length(unbox(%s)))" n))))
    (%set-string-len s n)
    s))

(define (copy-string s1 n)
  (let ((s2 (make-string n)))
    (cexp "box (memcpy (GET_STRING_POINTER(%s), GET_STRING_POINTER(%s), unbox(%s)))" s2 s1 n)
    s2))

(define (ascii->char n)
  (%make-immediate n #x02))

(define (char->ascii c)
  (cexp "box (GET_PAYLOAD(%s))" c))

(define (string-ref s n)
  (ascii->char (cexp "box(((pxll_string *)%s)->data[unbox(%s)])" s n)))

(define (string-set! s n c)
  (cexp "((pxll_string *)%s)->data[unbox(%s)] = GET_PAYLOAD (%s)" s n c))

(define (read fd bytes)
  (let ((buffer (make-string bytes)))
    (let ((r (cexp "box (read (unbox(%s), GET_STRING_POINTER(%s), unbox(%s)))" fd buffer bytes)))
      (if (%== r bytes)
	  buffer
	  (if (%lt? r bytes)
	      (copy-string buffer r)
	      (error "read() failed"))))))

(define (write fd s)
  (cexp "box (write (unbox(%s), GET_STRING_POINTER(%s), unbox(%s)))" fd s (%string-length s)))

(define (abuse-string s)
  (let ((slen (%string-length s)))
    (let loop ((n 0))
      (if (%== n slen)
	  #t
	  (begin
	    (string-set! s n (ascii->char (%+ (char->ascii (string-ref s n)) 1)))
	    (loop (%+ n 1)))))))

;; O_RDONLY == 0
(call/cc
 (lambda (exit)
   (set! error exit)
   (let ((fd (open "tests/fib.c" 0))
	 (sum 0)
	 )
     (let loop ((s (read fd 1024)) (sum 0))
       (let ((slen (%string-length s)))
	 (if (%zero? slen)
	     sum
	     (begin
	       (abuse-string s)
	       (write 2 s)
	       (loop (read fd 1024) (%+ sum slen)))))))
   ))

