;; -*- Mode: Scheme -*-

(cinclude "fcntl.h")

(define O_RDONLY (%%cexp int "O_RDONLY"))
(define O_WRONLY (%%cexp int "O_WRONLY"))
(define O_RDWR   (%%cexp int "O_RDWR"))
(define O_CREAT  (%%cexp int "O_CREAT"))

(define (open path oflag mode)
  (let ((fd (%%cexp (string int int -> int) "open (%s, %s, %s)" (zero-terminate path) oflag mode)))
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

;; file I/O 'object'

(define (file:open-read path)
  { fd = (open path O_RDONLY 0)
    buf = (make-string 16384)
    pos = 0
    end = 0 })

(define (file:open-write path create? mode)
  { fd = (open path (if create? (+ O_WRONLY O_CREAT) O_WRONLY) mode)
    buf = (make-string 16384)
    pos = 0 })

(define (file:close self)
  (close self.fd))

(define (file:fill-buffer self)
  (let ((n (read-into-buffer self.fd self.buf)))
    (set! self.end n)
    (set! self.pos 0)
    n))

(define (file:read-buffer self)
  (cond ((< self.pos self.end)
	 (let ((opos self.pos))
	   (set! self.pos self.end)
	   (substring self.buf opos self.end)))
	((= (file:fill-buffer self) 0) "")
	(else
	 (set! self.end 0)
	 (set! self.pos 0)
	 self.buf)))

(define (file:read-char self)
  (cond ((< self.pos self.end)
	 (set! self.pos (+ self.pos 1))
	 (string-ref self.buf (- self.pos 1)))
	((= (file:fill-buffer self) 0) #\eof)
	(else
	 (file:read-char self))))

(define (file:flush self)
  (let loop ((start 0))
    (let ((n (write-substring self.fd self.buf start self.pos)))
      (if (< n self.pos)
	  (loop n)
	  #u))))

(define (file:write-char self ch)
  (cond ((< self.pos (string-length self.buf))
	 (string-set! self.buf self.pos ch)
	 (set! self.pos (+ self.pos 1)))
	(else
	 (file:flush self)
	 (file:write-char self ch))))
