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
