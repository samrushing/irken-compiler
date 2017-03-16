;; -*- Mode: Irken -*-

(cinclude "fcntl.h")
(cinclude "unistd.h")

(define O_RDONLY (%%cexp int "O_RDONLY"))
(define O_WRONLY (%%cexp int "O_WRONLY"))
(define O_RDWR   (%%cexp int "O_RDWR"))
(define O_CREAT  (%%cexp int "O_CREAT"))
(define O_TRUNC  (%%cexp int "O_TRUNC"))

(define STDIN_FILENO  (%%cexp int "STDIN_FILENO"))
(define STDOUT_FILENO (%%cexp int "STDOUT_FILENO"))
(define STDERR_FILENO (%%cexp int "STDERR_FILENO"))

(define (open path oflag mode)
  (let ((path (zero-terminate path)))
    (syscall
     (%%cexp (string int int -> int)
	     "open (%0, %1, %2)"
	     path oflag mode))))

(define (read fd size)
  (let ((buffer (make-string size))
	(r (syscall (%%cexp (int string int -> int) "read (%0, %1, %2)" fd buffer size))))
    (if (= r size)
	buffer
	(copy-string buffer r))))

(define (read-into-buffer fd buffer)
  (syscall
   (%%cexp (int string int -> int)
	   "read (%0, %1, %2)"
	   fd buffer (string-length buffer)
	   )))

(define (write fd s)
  (syscall (%%cexp (int string int -> int) "write (%0, %1, %2)" fd s (string-length s))))

(define (write-substring fd s start len)
  ;; XXX range check
  (syscall (%%cexp (int string int int -> int) "write (%0, %1+%2, %3)" fd s start len)))

(define (read-stdin)
  (read 0 1024))

(define (close fd)
  (syscall (%%cexp (int -> int) "close (%0)" fd)))

;; file I/O 'object'

(define (file/open-read path)
  { fd = (open path O_RDONLY 0)
    buf = (make-string 16384)
    pos = 0
    end = 0 })

(define (file/open-write path create? mode)
  { fd = (open path (logior O_TRUNC (if create? (logior O_WRONLY O_CREAT) O_WRONLY)) mode)
    buf = (make-string 16384)
    pos = 0 })

(define (file/open-stdin)
  { fd = STDIN_FILENO
    buf = (make-string 16384)
    pos = 0
    end = 0 })

(define (file/open-stdout)
  { fd = STDOUT_FILENO
    buf = (make-string 16384)
    pos = 0 })

(define (file/close self)
  (close self.fd))

(define (file/fill-buffer self)
  (let ((n (read-into-buffer self.fd self.buf)))
    (set! self.end n)
    (set! self.pos 0)
    n))

(define (file/read-buffer self)
  (cond ((< self.pos self.end)
	 (let ((opos self.pos))
	   (set! self.pos self.end)
	   (substring self.buf opos self.end)))
	((= (file/fill-buffer self) 0) "")
	(else
	 (let ((r (substring self.buf self.pos self.end)))
	   (set! self.end 0)
	   (set! self.pos 0)
	   r))))

(define (file/read-char self)
  (cond ((< self.pos self.end)
	 (set! self.pos (+ self.pos 1))
	 (string-ref self.buf (- self.pos 1)))
	((= (file/fill-buffer self) 0) #\eof)
	(else
	 (file/read-char self))))

(define (file/flush self)
  (let loop ((start 0))
    (let ((n (write-substring self.fd self.buf start self.pos)))
      (if (< n self.pos)
	  (loop n)
	  #u))))

(define (file/write-char self ch)
  (cond ((< self.pos (string-length self.buf))
	 (string-set! self.buf self.pos ch)
	 (set! self.pos (+ self.pos 1)))
	(else
	 (file/flush self)
	 (file/write-char self ch))))

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
