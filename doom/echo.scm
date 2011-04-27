;; -*- Mode: Irken -*-

(include "doom/doom.scm")

(define (serve ip port)
  (let ((fd (socket AF_INET SOCK_STREAM 0))
        (addr (make-in-addr ip port)))
    (bind fd addr)
    (listen fd 5)
    (print-string "starting server...\n")
    (let loop ((cfd (accept fd)))
      (poller/fork (lambda () (client cfd)))
      (loop (accept fd)))))

(define (client fd)
  (print-string "client starting\n")
  (let loop ((s (recv fd 512)))
    (when (> (string-length s) 0)
          (send fd s)
          (loop (recv fd 512))))
  (print-string "exiting client...\n")
  (close fd)
  )

(serve "0.0.0.0" 9999)
(poller/wait-and-schedule)
