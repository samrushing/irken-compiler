;; -*- Mode: Irken -*-

;; file I/O 'objects' on C's stdio, with the same interface as the ones in
;;  lib/io.scm (which use posix open/read/write).  The stdio interface is
;;  the same on every platform, while posix's (O_CREAT etc.) is not: so the
;;  compiler and genffi use this one, and can run before ffi/posix_ffi.scm
;;  has been generated for this machine.
;;
;; differences from lib/io.scm:
;;  - the `mode` arguments are ignored: files are created with the usual
;;    0666 & ~umask.  (use lib/io.scm for files that need a restrictive
;;    mode, e.g. private keys.)
;;  - reads fill the whole buffer if they can (fread), so this is not
;;    the thing for interactive input.
;;  - lib/basis.scm includes lib/io.scm: use one or the other, not both.

(require-ffi 'stdio)

(define (stdio/raise-unless FILE*)
  (if (cref-null? FILE*)
      (raise-system-error)
      FILE*))

(define (file/fopen path mode)
  (stdio/raise-unless
   (stdio/fopen (%string->cref #f (zero-terminate path))
                (%string->cref #f (zero-terminate mode)))))

(define (file/fdopen fd mode)
  (stdio/raise-unless
   (stdio/fdopen fd (%string->cref #f (zero-terminate mode)))))

(define (file/open-read path)
  { fp = (file/fopen path "rb")
    rbuf = (make-string 16384)
    pos = 0
    end = 0 })

(define (file/open-write path create? mode)
  (when (not create?)
    ;; must already exist (as with lib/io.scm's open without O_CREAT).
    (stdio/fclose (file/fopen path "rb"))
    #u)
  { fp = (file/fopen path "wb")
    wbuf = (make-string 16384)
    pos = 0 })

(define (file/open-append path mode)
  { fp = (file/fopen path "ab")
    wbuf = (make-string 16384)
    pos = 0 })

(define (file/open-stdin)
  { fp = (file/fdopen 0 "rb")
    rbuf = (make-string 16384)
    pos = 0
    end = 0 })

(define (file/open-stdout)
  { fp = (file/fdopen 1 "wb")
    wbuf = (make-string 16384)
    pos = 0 })

(define (file/close self)
  (stdio/fclose self.fp))

;; write s[start:start+len] to FILE*.
(define (file/fwrite-substring FILE* s start len)
  (let ((s* (%string->cref #f s))
        (a* (%c-cast (array char) s*))
        (a0* (%c-aref char a* start)))
    (stdio/fwrite (%c-cast void (%c-cast char a0*)) 1 len FILE*)))

(define (file/fill-buffer self)
  (let ((n (stdio/fread (%c-cast void (%string->cref #f self.rbuf))
                        1 (string-length self.rbuf) self.fp)))
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
  (let loop ((line '()))
    (match (file/read-char file) with
      #\eof     -> (maybe:no)
      #\newline -> (maybe:yes (list->string (reverse line)))
      ch        -> (loop (list:cons ch line))
      )))

(define (file/read-lines f)
  (let loop ((lines '()))
    (match (file/read-line f) with
      (maybe:yes line) -> (loop (list:cons line lines))
      (maybe:no)       -> (reverse lines)
      )))

(define (file/flush self)
  (when (> self.pos 0)
    (when (< (file/fwrite-substring self.fp self.wbuf 0 self.pos) self.pos)
      (raise-system-error))
    (set! self.pos 0))
  (stdio/fflush self.fp)
  #u)

(define (file/write self s)
  (let ((slen (string-length s))
        (blen (string-length self.wbuf)))
    (cond ((< (+ self.pos slen) blen)
           (buffer-copy s 0 slen self.wbuf self.pos)
           (set! self.pos (+ self.pos slen)))
          (else
           (file/flush self)
           (when (< (file/fwrite-substring self.fp s 0 slen) slen)
             (raise-system-error))
           #u)
          )))

(define (file/write-char self ch)
  (cond ((< self.pos (string-length self.wbuf))
	 (string-set! self.wbuf self.pos ch)
	 (set! self.pos (+ self.pos 1)))
	(else
	 (file/flush self)
	 (file/write-char self ch))))

;; generates blocks, not characters.
(define (make-file-generator ifile)
  (make-generator
   (lambda (consumer)
     (let loop ((buf (file/read-buffer ifile)))
       (when (> (string-length buf) 0)
         (consumer (maybe:yes buf))
	 (loop (file/read-buffer ifile))))
     (forever (consumer (maybe:no))))))

(define (file-char-generator file)
  (make-generator
   (lambda (consumer)
     (let ((done #f))
       (while (not done)
         (let ((ch (file/read-char file)))
           (if (eq? ch #\eof)
               (set! done #t)
               (consumer (maybe:yes ch)))))
       (forever (consumer (maybe:no)))
       ))))

(defmacro with-file
  (with-file var file-exp exp ...)
  -> (let ((var file-exp)
           (result (begin exp ...)))
       (file/close var)
       result))
