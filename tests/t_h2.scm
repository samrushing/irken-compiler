;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")
(include "lib/rope.scm")
(include "lib/urandom.scm")
(include "lib/asn1/ber.scm")
(include "lib/codecs/base64.scm")
(include "lib/codecs/hex.scm")
(include "lib/crypto/ctbig.scm")
(include "lib/crypto/decaf.scm")
(include "lib/crypto/dh.scm")
(include "lib/crypto/hkdf.scm")
(include "lib/crypto/hmac.scm")
(include "lib/crypto/pem.scm")
(include "lib/crypto/sig.scm")
(include "lib/crypto/sodium.scm")
(include "lib/crypto/x509.scm")
(include "demo/bignum.scm")
(include "doom/doom.scm")
(include "doom/http/hpack.scm")
(include "doom/http/html.scm")
(include "doom/http/h2.scm")
(include "doom/tls/codec.scm")
(include "doom/tls/datatypes.scm")
(include "doom/tls/kex.scm")
(include "doom/tls/keysched.scm")
(include "doom/tls/signer.scm")
(include "doom/tls/tls13.scm")

(define thing-counter 0)

(define (hack a b)
  (:tuple a b))

(define format-header
  (:tuple name val) -> (html (dt name) (dd val))
  )

(define header->sexp
  (:tuple name val) -> (sexp (sym (string->symbol name)) (string val))
  )

;; XXX maybe use a multi-map for headers?
(define lookup-header
  key () -> (maybe:no)
  key ((:tuple name val) . rest)
  -> (if (string=? name key)
         (maybe:yes val)
         (lookup-header key rest)))

(define sexp-css
  (string-join
   (LIST
    "pre { line-height: 125%; }"
    "body  { background: #f8f8f8; }"
    "body .z { color: #408080; font-style: italic }"
    "body .C { color: #008000; font-weight: bold }"
    "body .s { color: #BA2121; }"
    "body .i { color: #000090; font-weight: bold }"
    "body .c { color: #800080; }"
    "body .p { color: #909090; }"
    "body .b { color: #0000C0; font-weight:bold }")
   "\n"))

(define (dummy-handler request)
  (let ((path (lookup-header ":path" request.headers))
        (conn (request.get-conn)))
    (if (match path with (maybe:yes path) -> (string=? path "/") (maybe:no) -> #f)
        (begin
          (request.send-headers
           (LIST (hack ":status" "200")
                 (hack "content-type" "text/html")) #t)
          (request.send-data
           (rope->string
            (html
             (html
              (head (style sexp-css))
              (body (h1 (&f "request " (int thing-counter)))
                    (h2 (&f "stream-id: " (int request.stream-id)))
                    (&pp (conn.get-info))
                    (&pp (sexp1 'request (map header->sexp request.headers)))
                    (p ((a (href "/")) "click here"))
                    ))))
           #t)
          (inc! thing-counter)
          )
        (request.send-headers (LIST (hack ":status" "404")) #f)
        )))

(define (fun-h2-conn sock h2-handler)
  (try
   ((make-h2-conn sock h2-handler))
   except
   (:Doom/EOF _)
   -> #u
   (:H2/BadFrame f)
   -> (printf "conn exit - bad frame\n")
   (:H2/BadPreface p)
   -> (printf "conn exit - bad preface: " (string p) "\n")
   )
  (sock.close)
  )

(define (h2-channel h2-handler x509s skey alpns)
  (lambda (sockfun)
    (try
     (fun-h2-conn (tls13/make-channel sockfun x509s skey alpns) h2-handler)
     except
     (:TLS/Failed)
     -> (printf "tls failed.\n")
     )))

;; handler := (lambda (sockfun) ...)
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
        (poller/fork (lambda () (handler sockfun)))
        (loop)))))

(define (read-certs path)
  (with-file file (file/open-read path)
    (let ((pgen (pem-gen (file-char-generator file)))
          (result '()))
      (for pem pgen
        (match pem with
          (:tuple "CERTIFICATE" asn1) -> (PUSH result asn1)
          _ -> (raise (:TLS/ExpectedCertificate path))
          ))
      (reverse result))))

(define (main)
  (let ((x509s (read-certs sys.argv[1]))
        (skey (read-skey sys.argv[2])))
    (printf "read " (int (length x509s)) " certs.\n")
    (serve "0.0.0.0" 4433 (h2-channel dummy-handler x509s skey '("h2")))
    (poller/wait-and-schedule)))

(main)
