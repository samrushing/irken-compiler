;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")
(include "doom/doom.scm")

(define (fetch-head ip)
  (let ((sock (doom/make (tcp4-sock)))
	(addr (address/make4 ip 80)))
    (doom/connect sock addr)
    (doom/send sock "HEAD / HTTP/1.0\r\n\r\n")
    (printf (string (doom/recv sock)) "\n")
    (doom/close sock)
    ))

(let ((ip "72.52.84.226"))
  (poller/fork (lambda () (fetch-head ip)))
  (poller/fork (lambda () (fetch-head ip)))
  (poller/fork (lambda () (fetch-head ip)))
  (poller/wait-and-schedule)
  )
