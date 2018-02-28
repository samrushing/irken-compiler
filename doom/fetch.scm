;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")
(include "doom/doom.scm")

(define (fetch-head ip)
  (let ((sock (doom/make (tcp4-sock)))
	(addr (address/make4 ip 80)))
    (sock.connect addr)
    (sock.send "HEAD / HTTP/1.0\r\n\r\n")
    (printf (string (sock.recv)) "\n")
    (sock.close)
    ))

(let ((ip "72.52.84.226"))
  (poller/fork (lambda () (fetch-head ip)))
  (poller/fork (lambda () (fetch-head ip)))
  (poller/fork (lambda () (fetch-head ip)))
  (poller/wait-and-schedule)
  )
