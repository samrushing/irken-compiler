;; -*- Mode: Irken -*-

(require "lib/basis.scm")
(require "lib/urandom.scm")
(require "lib/codecs/hex.scm")
(require "lib/codecs/base64.scm")
(require "lib/rope.scm")
(require "lib/asn1/ber.scm")
(require "lib/crypto/ctbig.scm")
(require "lib/crypto/pem.scm")
(require "lib/crypto/sig.scm")
(require "lib/crypto/sodium.scm")
(require "lib/crypto/decaf.scm")
(require "lib/crypto/dh.scm")
(require "lib/crypto/hmac.scm")
(require "lib/crypto/hkdf.scm")
(require "lib/crypto/x509.scm")

(require "demo/bignum.scm")
(require "doom/doom.scm")
(require "doom/tls/datatypes.scm")
(require "doom/tls/codec.scm")
(require "doom/tls/keysched.scm")
(require "doom/tls/kex.scm")
(require "doom/tls/signer.scm")
(require "doom/tls/tls13.scm")

(define (echo-client sock)
  (print-string "echo client starting\n")
  (try
   (let loop ()
     (let ((data (sock.recv)))
       (printf "data = " (string data) "\n")
       (when (> (string-length data) 0)
         (sock.send data)
         (loop))))
   except
   (:Doom/EOF _)
   -> #u)
  (printf "exiting client...\n")
  (sock.close)
  )

(require-ffi 'kqueue)

(define (serve ip port handler)
  (let ((sock (doom/make (tcp4-sock)))
        (addr (address/make4 ip port)))
    (sock.bind addr)
    (sock.listen 5)
    (printf "starting server...\n")
    (let loop ()
      (printf "accept wait...\n")
      (let (((sockfun addr) (sock.accept)))
        (printf "client: addr " (address/unparse addr) "\n")
        (poller/fork (lambda () (echo-client (handler sockfun))))
        (loop)))))

(define (read-certs path)
  (with-file file (file/open-read path)
    (let ((pgen (pem-gen (file-char-generator file)))
          (result '()))
      (for pem pgen
        (match pem with
          (:tuple "CERTIFICATE" asn1) -> (push! result asn1)
          _ -> (raise (:TLS/ExpectedCertificate path))
          ))
      (reverse result))))

;; to connect:
;; $ openssl s_client -connect hostname:4433 -alpn echo

(let ((x509s (read-certs sys.argv[1]))
      (skey (read-skey sys.argv[2])))
  (printf "read " (int (length x509s)) " certs.\n")
  (serve "0.0.0.0" 4433
         (lambda (sockfun)
           (tls13/make-channel sockfun x509s skey '("echo"))))
  (poller/wait-and-schedule))
