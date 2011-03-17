;; -*- Mode: Irken -*-

(define (system cmd)
  (%%cexp (string -> int)
	  "system (%0)"
	  (zero-terminate cmd)))

(define (cnull? val)
  (%%cexp (cstring -> bool) "%0==NULL" val))

(define (getenv name)
  (let ((val (%%cexp (string -> cstring)
		     "getenv (%0)"
		     (zero-terminate name))))
    (if (cnull? val)
	""
	(copy-cstring val))))

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

(cinclude "sys/utsname.h")

(define (uname)
  (let ((utsname (%callocate (struct utsname) 1)))
    (%%cexp ((buffer (struct utsname)) -> int) "uname (%0)" utsname)
    {sysname=  (copy-cstring (%%cexp ((buffer (struct utsname)) -> cstring) "%0->sysname" utsname))
     nodename= (copy-cstring (%%cexp ((buffer (struct utsname)) -> cstring) "%0->nodename" utsname))
     release=  (copy-cstring (%%cexp ((buffer (struct utsname)) -> cstring) "%0->release" utsname))
     version=  (copy-cstring (%%cexp ((buffer (struct utsname)) -> cstring) "%0->version" utsname))
     machine=  (copy-cstring (%%cexp ((buffer (struct utsname)) -> cstring) "%0->machine" utsname))
     }))
