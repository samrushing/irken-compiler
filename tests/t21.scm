;; file i/o

;; ok, looks like lookup_field() is borken, this won't work until it's fixed.

(include "lib/core.scm")
(include "lib/pair.scm")
(include "lib/string.scm")
(include "lib/io.scm")

(cinclude "fcntl.h")

(define O_RDONLY (%%cexp int "O_RDONLY"))
(define O_WRONLY (%%cexp int "O_WRONLY"))
(define O_RDWR   (%%cexp int "O_RDWR"))
(define O_CREAT  (%%cexp int "O_CREAT"))

(define (file:open-read path)
  (let ((fd (open path O_RDONLY 0))
	(buf (make-string 16384)))
    {fd=fd buf=buf pos=0 end=0}))

(define (file:open-write path create? mode)
  (let ((fd (open path (if create? (+ O_WRONLY O_CREAT) O_WRONLY) mode))
	(buf (make-string 16384)))
    {fd=fd buf=buf pos=0}))

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

;; could we instead define an 'eof' character?
(define (file:read-char self)
  (cond ((< self.pos self.end)
	 (let ((opos self.pos))
	   (set! self.pos (+ self.pos 1))
	   (:yes (string-ref self.buf (- self.pos 1)))))
	((= (file:fill-buffer self) 0) (:no))
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

;; copy file to stdout
(let ((f (file:open-read "gc.c")))
  (let loop ((buffer (file:read-buffer f)))
    (cond ((> (string-length buffer) 0)
	   (write 1 buffer)
	   (loop (file:read-buffer f)))))
  (file:close f))

;; read a few characters...
(let ((f (file:open-read "gc.c")))
  (let loop ((n 10))
    (cond ((= n 0) #t)
	  (else 
	   (printn (file:read-char f))
	   (loop (- n 1)))))
  (file:close f))

;; write a file by chars...
(let ((f (file:open-write "thing.txt" #t #o644)))
  ;;(print-string "fd=") (printn f.fd)
  (file:write-char f #\H)
  (file:write-char f #\o)
  (file:write-char f #\w)
  (file:write-char f #\d)
  (file:write-char f #\y)
  (file:write-char f #\newline)
  (file:flush f)
  (file:close f)
  )
  
