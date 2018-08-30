;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")
(include "lib/urandom.scm")
(include "lib/codecs/hex.scm")
(include "lib/codecs/base64.scm")
(include "lib/rope.scm")
(include "lib/asn1/ber.scm")
(include "lib/crypto/ctbig.scm")
(include "lib/crypto/pem.scm")
(include "lib/crypto/sig.scm")
(include "lib/crypto/sodium.scm")
(include "lib/crypto/decaf.scm")
(include "lib/crypto/dh.scm")
(include "lib/crypto/hmac.scm")
(include "lib/crypto/hkdf.scm")
(include "lib/crypto/x509.scm")

(include "demo/bignum.scm")
(include "doom/doom.scm")
(include "doom/tls/datatypes.scm")
(include "doom/tls/codec.scm")
(include "doom/tls/keysched.scm")
(include "doom/tls/kex.scm")
(include "doom/tls/signer.scm")
(include "doom/tls/tls13.scm")

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
