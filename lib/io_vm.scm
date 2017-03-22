;; -*- Mode: Irken -*-

(include "lib/posix.scm")

;; sadly, these constants are not the same across
;;   various systems.

;; quick python hack to fetch values:
;; for x in dir(os):
;;   if x.startswith('O_'):
;;     print x, getattr (os, x)

(define O_RDONLY    0)
(define O_WRONLY    0)
(define O_RDWR      0)
(define O_CREAT     0)
(define O_TRUNC     0)
(define O_EXCL      0)
(define O_APPEND    0)
(define O_NONBLOCK  0)

;; candidate for `lib/derived.scm`
(defmacro set-variables!
  (set-variables! pairs ...)
  -> (begin (set-variables* pairs ...)))

(defmacro set-variables*
  (set-variables* (name val))
  -> (set! name val)
  (set-variables* (name val) pairs ...)
  -> (begin (set! name val) (set-variables* pairs ...))
  )

(define (set-open-flags!)
  (let ((info (uname)))
    (match info.sysname with
      "Linux"
      -> (set-variables!
          (O_RDONLY         0)
          (O_WRONLY         1)
          (O_RDWR           2)
          (O_CREAT         64)
          (O_EXCL         128)
          (O_TRUNC        512)
          (O_APPEND      1024)
          (O_NONBLOCK  131072)
          )
      "Darwin"
      -> (set-variables!
          (O_RDONLY         0)
          (O_WRONLY         1)
          (O_RDWR           2)
          (O_CREAT        512)
          (O_EXCL        2048)
          (O_TRUNC       1024)
          (O_APPEND         8)
          (O_NONBLOCK       4))
      "FreeBSD"
      -> (set-variables!
          (O_RDONLY         0)
          (O_WRONLY         1)
          (O_RDWR           2)
          (O_CREAT        512)
          (O_EXCL        2048)
          (O_TRUNC       1024)
          (O_APPEND         8)
          (O_NONBLOCK       4))
      sysname
      -> (raise (:UnknkownSystem sysname))
      )))

(set-open-flags!)

(define posix-open
  (make-ffi "open\x00" #\i 3 (path oflag mode) (string int int -> int)))

(define (open path oflag mode)
  (let ((path (zero-terminate path)))
    (syscall (posix-open path oflag mode))))

(define posix-read
  (make-ffi "read\x00" #\i 3 (fd buffer size) (int string int -> int)))

(define (read fd size)
  (let ((s (make-string size))
        (r (syscall (posix-read fd s size))))
    (if (= r size)
	s
	(copy-string s r))))

(define (read-into-buffer fd buffer)
  (syscall (posix-read fd buffer (string-length buffer))))

(define posix-write
  (make-ffi "write\x00" #\i 3 (fd buffer size) (int string int -> int)))

(define (write fd s)
  (syscall (posix-write fd s (string-length s))))

(define (write-substring fd s start len)
  ;; Note: this does a copy of the substring rather than something smarter.
  (let ((ss (substring s start (+ start len))))
    (write fd ss)))

(define posix-close
  (make-ffi "close\x00" #\i 1 (fd) (int -> int)))

(define (close fd)
  (syscall (posix-close fd)))
