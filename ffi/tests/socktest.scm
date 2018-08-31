;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")
(include "lib/net/socket.scm")

(define (test-echo addr0 sock0)
  (printf "fd0 = " (int sock0.fd) "\n")
  (sock/bind sock0 addr0)
  (printf "bind ok.\n")
  (sock/listen sock0 5)
  (printf "listen ok.\n")
  (match (sock/accept sock0) with
    (:tuple sock1 addr1)
    -> (begin
         (printf "accept ok.\n")
         (printf "peer address: " (address/unparse addr1) "\n")
         (let ((buf (buffer/make 4096)))
           (for-range i 5
             (sock/recv sock1 buf)
             (printf (string (buffer/contents buf)) "\n")
             (sock/send sock1 buf)
             (buffer/reset! buf)
             )
           (sock/close sock1)
           )))
  (sock/close sock0)
  )

(test-echo (address/make4 "127.0.0.1" 9002) (tcp4-sock))
(test-echo (address/make6 "::1" 9002) (tcp6-sock))
