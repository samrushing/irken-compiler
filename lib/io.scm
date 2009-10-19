;; -*- Mode: Scheme -*-

(define (open path oflag mode)
  (let ((fd (%%cexp (string int int -> int) "open (%s, %s, %s)" path oflag mode)))
    (if (>= fd 0)
	fd
	(error "open() failed"))))

(define (read fd size)
  (let* ((buffer (make-string size))
	 (r (%%cexp (int string int -> int) "read (%s, %s, %s)" fd buffer size)))
    (if (= r size)
	buffer
	(if (< r size)
	    (copy-string buffer r)
	    (error "read() failed")))))

(define (read-into-buffer fd buffer)
  (let* ((size (string-length buffer))
	 ;; XXX range check
	 (r (%%cexp (int string int -> int) "read (%s, %s, %s)" fd buffer size)))
    r))

(define (write fd s)
  (%%cexp (int string int -> int) "write (%s, %s, %s)" fd s (string-length s)))

(define (write-substring fd s start len)
  ;; XXX range check
  (%%cexp (int string int int -> int) "write (%s, %s+%s, %s)" fd s start len))

(define (close fd)
  (%%cexp (int -> int) "close (%s)" fd))

(class readfile

  (fd:int buffer:string pos:int end:int)

  (define (fill-buffer self)
    (let ((n (read-into-buffer self.fd self.buffer)))
      (set! self.end n)
      (set! self.pos 0)
      n))
	     
  (define (read-buffer self)
    ;; get an entire buffer-full at a time
    (cond ((< self.pos self.end)
	   (let ((opos self.pos))
	     (set! self.pos self.end)
	     (substring self.buffer opos self.end)))
	  ((= (self.read-buffer) 0) "")
	  (else
	   (set! self.end 0)
	   (set! self.pos 0)
	   self.buffer)))

  (define (read-char self)
    (cond ((< self.pos self.end)
	   (set! self.pos (+ self.pos 1))
	   (maybe/yes (string-ref self.buffer (- self.pos 1))))
	  ((= (self.read-buffer) 0)
	   (maybe/no))
	  (else
	   (self.read-char))))
  )

(define (open-readfile path buffer-size)
  (readfile (open path 0 0) (make-string buffer-size) 0 0))

