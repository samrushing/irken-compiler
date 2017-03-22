;; -*- Mode: Irken -*-

(define (system cmd)
  (%%cexp (string -> int)
	  "system (%0)"
	  (zero-terminate cmd)))

(define (abort)
  (%%cexp (-> undefined)
	  "(object)3; abort()"))

(define (tmpnam)
  (copy-cstring (%%cexp (-> cstring) "tmpnam(NULL)")))

(define (cnull? val)
  (%%cexp (cstring -> bool) "%0==NULL" val))

(define (getenv name)
  (let ((val (%%cexp (string -> cstring)
		     "getenv (%0)"
		     (zero-terminate name))))
    (if (cnull? val)
	""
	(copy-cstring val))))

(define (unlink name)
  (%%cexp (string -> int)
	  "unlink (%0)"
	  (zero-terminate name)))

(define sys
  (let ((argc (%%cexp (-> int) "argc"))
	(argv 
	 (let ((v (make-vector argc "")))
	   (define (get-arg n)
	     (copy-cstring (%%cexp (int -> cstring) "argv[%0]" n)))
	   (let loop ((n argc))
	     (cond ((zero? n) v)
		   (else
		    (set! v[(- n 1)] (get-arg (- n 1)))
		    (loop (- n 1))))))))
  { argc=argc argv=argv }
  ))

