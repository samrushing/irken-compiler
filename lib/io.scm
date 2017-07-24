;; -*- Mode: Irken -*-

(require-ffi 'posix)

(define (open path oflag mode)
  (syscall
   (posix/open (%string->cref #f (zero-terminate path)) oflag mode)))

(define (read fd size)
  (let ((buffer (make-string size))
        (r (syscall (posix/read fd (%string->cref #f buffer) size))))
    (if (= r size)
	buffer
	(copy-string buffer r))))

(define (read-into-buffer fd buffer)
  (let ((blen (string-length buffer)))
    (syscall
     (posix/read fd (%string->cref #f buffer) blen)
     )))

(define (write fd s)
  (let ((slen (string-length s)))
    (syscall (posix/write fd (%string->cref #f s) slen))
    ))

;; XXX perhaps %string->cref should return (array char)?
(define (write-substring fd s start len)
  (let ((s* (%string->cref #f s))
        (a* (%c-cast (array char) s*))
        (a0* (%c-aref char a* start)))
    ;; XXX range check
    (syscall (posix/write fd (%c-cast char a0*) len))
    ))

(define (read-stdin)
  (read 0 1024))

(define (close fd)
  (syscall (posix/close fd)))

;; file I/O 'object'

(define (file/open-read path)
  { fd = (open path O_RDONLY 0)
    rbuf = (make-string 16384)
    pos = 0
    end = 0 })

(define (file/open-write path create? mode)
  { fd = (open path (logior O_TRUNC (if create? (logior O_WRONLY O_CREAT) O_WRONLY)) mode)
    wbuf = (make-string 16384)
    pos = 0 })

(define (file/open-stdin)
  { fd = STDIN_FILENO
    rbuf = (make-string 16384)
    pos = 0
    end = 0 })

(define (file/open-stdout)
  { fd = STDOUT_FILENO
    wbuf = (make-string 16384)
    pos = 0 })

(define (file/close self)
  (close self.fd))

(define (file/fill-buffer self)
  (let ((n (read-into-buffer self.fd self.rbuf)))
    (set! self.end n)
    (set! self.pos 0)
    n))

(define (file/read-buffer self)
  (cond ((< self.pos self.end)
	 (let ((opos self.pos))
	   (set! self.pos self.end)
	   (substring self.rbuf opos self.end)))
	((= (file/fill-buffer self) 0) "")
	(else
	 (let ((r (substring self.rbuf self.pos self.end)))
	   (set! self.end 0)
	   (set! self.pos 0)
	   r))))

(define (file/read-char self)
  (cond ((< self.pos self.end)
	 (set! self.pos (+ self.pos 1))
	 (string-ref self.rbuf (- self.pos 1)))
	((= (file/fill-buffer self) 0) #\eof)
	(else
	 (file/read-char self))))

(define (file/read-line file)
  (let loop ((ch (file/read-char file))
         (r '()))
    (if (eq? ch #\newline)
        (list->string (reverse r))
        (loop (file/read-char file) (list:cons ch r)))))

(define (file/flush self)
  (let loop ((start 0))
    (let ((n (write-substring self.fd self.wbuf start self.pos)))
      (if (< n self.pos)
	  (loop n)
	  (set! self.pos 0))
      )))

(define (file/write self s)
  (let ((slen (string-length s))
        (blen (string-length self.wbuf)))
    (cond ((< (+ self.pos slen) blen)
           (buffer-copy s 0 slen self.wbuf self.pos)
           (set! self.pos (+ self.pos slen)))
          (else
           (file/flush self)
           (write self.fd s)
           #u)
          )))

(define (file/write-char self ch)
  (cond ((< self.pos (string-length self.wbuf))
	 (string-set! self.wbuf self.pos ch)
	 (set! self.pos (+ self.pos 1)))
	(else
	 (file/flush self)
	 (file/write-char self ch))))

(define (make-file-generator ifile)
  (make-generator
   (lambda (consumer)
     (let loop ((buf (file/read-buffer ifile)))
       (if (= 0 (string-length buf))
           (consumer (maybe:no))
           (consumer (maybe:yes buf)))
       (loop (file/read-buffer ifile))))))
