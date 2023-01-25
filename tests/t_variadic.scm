;; -*- Mode: Irken -*-

(require "lib/basis.scm")

(require-ffi 'vari)

(define (vopen path oflag mode)
  (syscall
   (vari/open (%string->cref #f path) oflag mode)))

(let ((fd (vopen "/tmp/test.txt\x00" 1537 #o644)))
  (printf "fd = " (int fd) "\n")
  fd)


