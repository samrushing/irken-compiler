;; -*- Mode: Irken -*-

(include "doom/doom.scm")

(define (fetch-head ip)
  (let ((sfd (socket AF_INET SOCK_STREAM 0))
	(addr (make-in-addr ip 80)))
    (connect sfd addr)
    (printn (send sfd "HEAD / HTTP/1.0\r\n\r\n"))
    (print-string (recv sfd 1024))
    (print-string "done!\n")
    (close sfd)
    ))

(let ((ip "72.52.84.226"))
  (poller/fork (lambda () (fetch-head ip)))
  (poller/fork (lambda () (fetch-head ip)))
  (poller/fork (lambda () (fetch-head ip)))
  (poller/wait-and-schedule)
  )
