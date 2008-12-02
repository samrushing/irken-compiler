;; -*- Mode: Scheme -*-

(define (open path flags)
  (%%verify "TC_STRING" 1 path)
  (%%verify "TC_INT" 1 flags)
  (let ((fd (%%cexp "box (open (GET_STRING_POINTER(%s), unbox(%s)))" path flags)))
    (if (%ge? fd 0)
	fd
	(error "open() failed"))))

(define (read fd size)
  (%%verify "TC_INT" 1 fd)
  (let* ((buffer (make-string size))
	 (r (%%cexp "box (read (unbox(%s), GET_STRING_POINTER(%s), unbox(%s)))" fd buffer size)))
    (if (= r size)
	buffer
	(if (< r size)
	    (copy-string buffer r)
	    (error "read() failed")))))

(define (read-into-buffer fd buffer)
  (%%verify "TC_INT" 1 fd)
  (let* ((size (string-length buffer))
	 (r (%%cexp "box (read (unbox(%s), GET_STRING_POINTER(%s), unbox(%s)))" fd buffer size)))
    r))

(define (write fd s)
  (%%verify "TC_INT" 1 fd)
  (%%verify "TC_STRING" 1 s)
  (%%cexp "box (write (unbox(%s), GET_STRING_POINTER(%s), unbox(%s)))" fd s (string-length s)))

(define (write-substring fd s start len)
  ;; XXX range check!
  (%%verify "TC_INT" 1 fd)
  (%%verify "TC_STRING" 1 s)
  (%%verify "TC_INT" 1 start)
  (%%verify "TC_INT" 1 len)
  (%%cexp "box (write (unbox(%s), GET_STRING_POINTER(%s)+unbox(%s), unbox(%s)))" fd s start len))

(define (close fd)
  (%%cexp "box (close (unbox (%s)))" fd))

(define eof-object 'end-of-file)

;; this message-based OO will do until we get to the plans in lib/uobj.scm

(define (buffered-file fd buffer-size)

  ;; meant to be used for *either* reading *or* writing, not both.   ya dingbat.

  ;; [-----<pos>XXXXXXXXX<end>------]
  
  (let ((buffer (make-string buffer-size))
        (pos 0)
        (end 0))

    (define (read-buffer)
      (let ((n (read-into-buffer fd buffer)))
	;;(print-string "read ") (%print n) (print-string " bytes into buffer\n")
        (cond ((= n 0) eof-object)
              ((> n 0)
               (set! end n)
               (set! pos 0))
              (else
               (error "read(2) failed")))))

    (define (read-buffer*)
      ;; get an entire buffer-full at a time
      (cond ((< pos end)
	     (let ((opos pos))
	       (set! pos end)
	       (substring buffer opos end)))
	    ((eq? (read-buffer) eof-object)
	     eof-object)
	    (else
	     (read-buffer*))))

    (define (write-buffer)
      (let loop ()
        (if (< pos end)
            (let ((n (write-substring buffer fd pos (- end pos))))
	      (cond ((> n 0)
		     (set! pos (+ pos n))
		     (loop))
		    (else
		     (error "write(2) failed"))))))
      (set! pos 0)
      (set! end 0)
      )

    (define (read-char)
      (cond ((< pos end)
             (let ((ch (string-ref buffer pos)))
               (set! pos (+ pos 1))
               ch))
            (else
             (if (eq? (read-buffer) eof-object)
		 eof-object
                 (read-char)))))

    (define (read-line)
      (let oloop ((buffers '()))
	(let iloop ((i pos))
	  (cond ((= i end)
		 ;; fetch a new buffer
		 (let ((piece (substring buffer pos end)))
		   (read-buffer)
		   (oloop (cons piece buffers))))
		((eq? (string-ref buffer i) #\newline)
		 (let ((result (substring buffer pos i)))
		   (set! pos i)
		   (reverse (cons result buffers))))
		(else
		 (iloop (+ i 1)))))))

    (define (write-char ch)
      (cond ((< end buffer-size)
             (string-set! buffer end ch)
             (set! end (+ end 1)))
            (else
             (write-buffer)
             (write-char ch))))

    (define (write-string s)
      (let ((sl (string-length s)))
        (cond ((> (+ sl end) buffer-size)
               ;; just flush then write the string
               (write-buffer)
               (write fd s))
              (else
               (buffer-copy s 0 sl buffer end)
               (set! end (+ end sl))))))

    (define (close*)
      (flush)
      ;; little issue with names here, should probably call
      ;; the direct os interfaces os-open, os-read, etc...
      (close fd))

    (define (flush)
      (write-buffer))

    ;; simple message dispatcher
    (lambda (msg)
      (case msg
	((read-char) read-char)
	((read-buffer) read-buffer*)
	((write-char) write-char)
	((write-string) write-string)
	((write-buffer) write-buffer)	
	((flush) flush)
	((close) close*)
	(else (error "no such method"))))
    )
  )
