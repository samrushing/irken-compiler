;; -*- Mode: Irken -*-

;; sendto(2) and recvfrom(2) take six arguments: more than the native
;;  backends could once pass to C.  send a datagram to ourselves.

(require "lib/basis.scm")
(require "lib/net/socket.scm")

(let ((sock (udp4-sock))
      (addr (address/make4 "127.0.0.1" 47321))
      (msg "hello from sendto")
      (out (buffer/make 64))
      (in (buffer/make 64))
      (from (address/make))
      (fromlen* (halloc uint)))
  (sock/bind sock addr)
  (buffer/add! out msg)
  (let ((sent (syscall (socket/sendto sock.fd (%c-cast void out.buf) (string-length msg) 0
                                      addr.addr addr.size))))
    (c-set-int fromlen* from.size)
    (let ((got (syscall (socket/recvfrom sock.fd (%c-cast void in.buf) in.size 0
                                         from.addr fromlen*))))
      (set! in.end got)
      (printf "sent " (int sent) " got " (int got) " "
              (string (buffer/contents in)) " from " (address/unparse from) "\n"))))
