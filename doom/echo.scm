;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")
(include "doom/doom.scm")

(require-ffi 'kqueue)

(define (serve ip port)
  (let ((sock (doom/make (tcp4-sock) 0 0))
        (addr (address/make4 ip port)))
    (doom/bind sock addr)
    (doom/listen sock 5)
    (printf "starting server...\n")
    (let loop ()
      (let (((csock addr) (doom/accept sock)))
        (printf "client fd " (int csock.sock.fd) " addr " (address/unparse addr) "\n")
        (poller/fork (lambda () (client csock)))
        (loop)))))

(define (client sock)
  (print-string "client starting\n")
  (let loop ()
    (let ((data (doom/recv sock)))
      (printf "data = " (string data) "\n")
      (when (> (string-length data) 0)
        (doom/send sock data)
        (loop))))
  (printf "exiting client...\n")
  (doom/close sock)
  )

(serve "0.0.0.0" 9999)
(poller/wait-and-schedule)
