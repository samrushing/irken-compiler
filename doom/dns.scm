;; -*- Mode: Irken -*-

(require "lib/basis.scm")
(require "lib/net/dns.scm")
(require "doom/doom.scm")

(define (go ip qtype name)
  (let ((sock (doom/make (udp4-sock) 1024 1024))
        (addr (address/make4 ip 53))
        (packet (build-question 3141 name qtype dnsclass-in 1)))
    (try
     (begin
       (sock.connect addr)
       (sock.send packet)
       (let ((reply (sock.recv))
             (unpacked (unpack-reply reply)))
         (print-reply unpacked)
         (sock.close)))
     except
     (:DNSTruncated)
     -> (begin
          (printf "trying tcp...\n")
          (sock.close)
          (set! sock (doom/make (tcp4-sock) 8192 1024))
          (sock.connect addr)
          (sock.send (string-append (tcp-enc-size-prefix (string-length packet)) packet))
          (let ((size (tcp-dec-size-prefix (sock.recv-exact 2)))
                (_ (printf "tcp reply size = " (int size) "\n"))
                (reply (sock.recv-exact size))
                (unpacked (unpack-reply reply)))
            (print-reply unpacked)
            (sock.close))
          )
     )))

;; this really needs either getopt, or pattern matching on vectors.

(let ((server "127.0.0.1")
      (qtype (dnstype:any))
      (name "nightmare.com"))
  (match (vector-length sys.argv) with
    1 -> #u
    2 -> (set! name sys.argv[1])
    3 -> (begin (set! server sys.argv[1]) (set! name sys.argv[2]))
    4 -> (begin (set! server sys.argv[1]) (set! qtype (name->dnstype sys.argv[2])) (set! name sys.argv[3]))
    _ -> (begin (printf "usage: " sys.argv[0] " <name> | <server-ip> <name> | <server-ip> <qtype> <name>\n") (%exit #f -1))
    )
  (poller/fork (lambda () (go server qtype name)))
  (poller/wait-and-schedule)
  )
