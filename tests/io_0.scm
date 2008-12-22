(include "lib/core.scm")
(include "lib/string.scm")

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

(print-string "opening ") (printn (sys:argv 1))
(let ((fd (open (sys:argv 1) 0 0))
      (buffer (make-string 100)))
  (printn (read fd 100))
  (printn (read-into-buffer fd buffer))
  )
(printn "tests/out.txt\x00")
(let ((fd (open "tests/out.txt\x00" #x202 #o777)))
  (printn (write fd "this is a test\n"))
  (close fd)
  )
