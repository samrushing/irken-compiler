;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")
(include "doom/doom.scm")
(include "lib/codecs/base64.scm")
(include "lib/crypto/sha1.scm")
(include "doom/http/websocket.scm")

(define (ws-echo-client sockfun)
  (let ((ws (websocket (sockfun 8192 8192))))
    (for pkt ws.pktgen
      (match pkt with
        (:tuple opcode data fin?)
        -> (ws.send-packet opcode data fin?)))
    (printf "exiting client...\n")
    (ws.close)
    ))

(define (serve ip port)
  (let ((sock (doom/make (tcp4-sock)))
        (addr (address/make4 ip port)))
    (sock.bind addr)
    (sock.listen 5)
    (printf "starting server...\n")
    (let loop ()
      (printf "accept wait...\n")
      (let (((sockfun addr) (sock.accept)))
        (printf "client: addr " (address/unparse addr) "\n")
        (poller/fork (lambda () (ws-echo-client sockfun)))
        (loop)))))

(serve "0.0.0.0" 9999)
(poller/wait-and-schedule)
