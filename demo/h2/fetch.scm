;; -*- Mode: Irken -*-

;; fetch a URL over HTTP/2 (or HTTP/1.1, if that's all the server
;;  speaks) and TLS 1.3, checking the server's certificate.
;;
;; $ demo/h2/fetch https://dark.nightmare.com:8443/
;;
;; certificates are checked against the system's trusted roots, or
;;  against -root <file.pem> (which may hold a root, or the server's
;;  own certificate, to pin it).  see lib/crypto/x509path.scm for what
;;  is (and isn't) checked.

(require "lib/basis.scm")
(require "lib/getopt.scm")
(require "lib/time.scm")
(require "lib/posix.scm")
(require "lib/net/dns.scm")
(require "lib/codecs/zlib.scm")
(require "doom/doom.scm")
(require "doom/tls/client.scm")
(require "doom/http/h2client.scm")
(require "doom/http/h1client.scm")

(define program-options
  (makeopt
   (flag 'v)
   (flag 'i)
   (flag 'k)
   (arg 'root (string ""))
   (arg 'ip (string ""))
   (pos 'url (string ""))
   (flag 'h)
   ))

(define (usage)
  (printf "fetch a URL with HTTP/2 (or HTTP/1.1) over TLS 1.3.\n\n"
          "Usage: " sys.argv[0] " [options] https://host[:port]/path\n"
          "  [-i]            print the response status and headers\n"
          "  [-v]            print the TLS connection details and certificates\n"
          "  [-root <pem>]   trust these certificates instead of the system's roots\n"
          "  [-k]            don't check the server's certificate (insecure!)\n"
          "  [-ip <addr>]    connect to this IPv4 address instead of looking up the host\n"
          "  [-h]            print this usage.\n")
  (%exit #f 2))

(define (die msg)
  (printf "fetch: " msg "\n")
  (%exit #f 1))

;; https://host[:port][/path] -> {host port path}
(define (parse-url url)
  (if (not (starts-with url "https://"))
      (die "only https:// URLs are supported")
      (let ((rest (substring url 8 (string-length url)))
            (slash (string-find "/" rest))
            (hostport (if (= slash -1) rest (substring rest 0 slash)))
            (path (if (= slash -1) "/" (substring rest slash (string-length rest)))))
        (match (string-split hostport #\:) with
          (host)      -> {host=host port=443 path=path}
          (host port) -> {host=host port=(string->int port) path=path}
          _           -> (die (format "bad URL: " url))
          ))))

;; the first nameserver in /etc/resolv.conf
(define (find-nameserver)
  (let ((r (maybe:no)))
    (when (isfile? "/etc/resolv.conf")
      (with-file f (file/open-read "/etc/resolv.conf")
        (for-list line (file/read-lines f)
          (match (string-split (string-trim-both line) #\space) with
            ("nameserver" ip)
            -> (when (and (eq? r (maybe:no)) (ipv4-literal? ip))
                 (set! r (maybe:yes ip)))
            _ -> #u))))
    (match r with
      (maybe:yes ip) -> ip
      (maybe:no)     -> (die "no IPv4 nameserver in /etc/resolv.conf"))))

;; look up an A record (following CNAMEs is the resolver's job).
(define (resolve host)
  (if (ipv4-literal? host)
      host
      (let ((sock (doom/make (udp4-sock) 4096 1024)))
        (sock.connect (address/make4 (find-nameserver) 53))
        (sock.send (build-question 1 host (dnstype:a) dnsclass-in 1))
        (let (((header reply) (unpack-reply (sock.recv)))
              (r (maybe:no)))
          (sock.close)
          (for-list rr reply.an
            (match rr.data with
              (dnsrr:a bytes)
              -> (when (eq? r (maybe:no))
                   (set! r (maybe:yes (format (join int->string "." (map char->int (vector->list bytes)))))))
              _ -> #u))
          (match r with
            (maybe:yes ip) -> ip
            (maybe:no)     -> (die (format "can't find an address for " host)))))))

(define (load-anchors path)
  (if (> (string-length path) 0)
      (x509-load-anchors path)
      (let loop ((paths x509-system-anchor-paths))
        (match paths with
          () -> (die "can't find the system's trusted roots: use -root")
          (p . rest) -> (if (isfile? p) (x509-load-anchors p) (loop rest))))))

(define (print-headers resp)
  (printf (bold "status " resp.status) "\n")
  (for-list h resp.headers
    (match h with
      (:tuple name val) -> (printf name ": " val "\n")))
  (printf "\n"))

;; does <s> start with a zlib (rfc1950) header?
(define (zlib-header? s)
  (and (>= (string-length s) 2)
       (let ((b0 (char->int (string-ref s 0)))
             (b1 (char->int (string-ref s 1))))
         (and (= 8 (logand b0 #x0f))           ;; CM = deflate
              (= 0 (mod (+ (* b0 256) b1) 31)))))) ;; FCHECK

(define (inflate-string s window-bits)
  (string-concat (generator->list (inflate2 (list-generator (list s)) 16384 window-bits))))

;; "deflate" is supposed to mean zlib-wrapped (rfc9110 8.4.1.2), but
;;  some servers send raw deflate.
(define (decode-body resp)
  (try
   (match (h2/header "content-encoding" resp.headers) with
     (maybe:yes "deflate")
     -> (if (zlib-header? resp.body)
            ;; raw deflate can look like a zlib header (by chance, 1 in ~500)
            (try
             (inflate-string resp.body 15)
             except
             (:Zlib/APIFailure _) -> (inflate-string resp.body -15))
            (inflate-string resp.body -15))
     (maybe:yes "gzip")
     -> (inflate-string resp.body 31)
     _ -> resp.body
     )
   except
   (:Zlib/APIFailure _) -> (die "can't decompress the response body")))

(define (fetch url opts)
  (let ((u (parse-url url))
        (verbose? (match (get-bool-opt opts 'v) with (maybe:yes v) -> v _ -> #f))
        (headers? (match (get-bool-opt opts 'i) with (maybe:yes v) -> v _ -> #f))
        (insecure? (match (get-bool-opt opts 'k) with (maybe:yes v) -> v _ -> #f))
        (root (match (get-string-opt opts 'root) with (maybe:yes v) -> v _ -> ""))
        (ip (match (get-string-opt opts 'ip) with
              (maybe:yes v) -> (if (> (string-length v) 0) v (resolve u.host))
              _ -> (resolve u.host)))
        (anchors (if insecure? (list:nil) (load-anchors root)))
        (now (let ((t (gettime/realtime))) t.sec))
        (authority (if (= u.port 443) u.host (format u.host ":" (int u.port)))))
    (define (verify ders)
      (if insecure?
          (begin
            (printf (ansi (bold red) "warning:") " not checking the server's certificate\n")
            (parse-x509 (first ders)))
          (x509-verify-server ders anchors u.host now)))
    (when verbose?
      (printf "* connecting to " ip ":" (int u.port)
              (if insecure? "" (format " (" (int (length anchors)) " trusted roots)")) "\n"))
    (let ((sock (doom/make (tcp4-sock))))
      (sock.connect (address/make4 ip u.port))
      (let ((tls (try
                  (tls13/client sock u.host '("h2" "http/1.1") verify)
                  except
                  (:TLS/ClientFailed why) -> (die (format "TLS: " why))))
            (info (tls.get-info)))
        (when verbose?
          (printf "* TLS 1.3 " (sym info.suite) ", " (sym info.group) ", " info.sigalg "\n")
          (for-range i (length info.certs)
            (let ((x (parse-x509 (nth info.certs i))))
              (printf "*   [" (int i) "] " (x509-name-repr x.cert.subject) "\n"
                      "*       issued by " (x509-name-repr x.cert.issuer) "\n")))
          (flush)
          #u)
        (let ((headers (list (:tuple "user-agent" "irken-fetch/0.1")
                             (:tuple "accept" "*/*")
                             (:tuple "accept-encoding" "gzip, deflate")))
              (h2? (match info.alpn with
                     (maybe:yes "h2") -> #t
                     ;; "http/1.1", or no ALPN at all
                     _ -> #f))
              (_ (when verbose?
                   (printf "* " (if h2? "HTTP/2" "HTTP/1.1") "\n")))
              (resp (try
                     (if h2?
                         (let ((h2 (h2/client tls)))
                           (h2.request "GET" authority u.path headers))
                         (h1/request tls "GET" authority u.path headers))
                     except
                     (:H2/GoAway code debug)
                     -> (die (format "HTTP/2: server sent GOAWAY " (h2-error-name code) " " debug))
                     (:H2/StreamReset code)
                     -> (die (format "HTTP/2: server reset the stream: " (h2-error-name code)))
                     (:H2/BadFrame what)
                     -> (die (format "HTTP/2: bad frame: " what))
                     (:H1/BadResponse what)
                     -> (die (format "HTTP/1.1: " what))
                     (:Doom/EOF _)
                     -> (die "connection closed by server"))))
          (when (or verbose? headers?)
            (print-headers resp))
          (print-string (decode-body resp))
          (tls.close)
          (%exit #f (if (eq? (string-ref resp.status 0) #\2) 0 1)))))))

(define (main)
  (set-verbose-gc #f)
  (let ((opts (try
               (process-options program-options (rest (vector->list sys.argv)))
               except
               (:Getopt/MissingArg _ _) -> (usage)
               (:Getopt/UnknownArg _ _) -> (usage))))
    (when-maybe v (get-bool-opt opts 'h) (usage))
    (match (get-string-opt opts 'url) with
      (maybe:yes url)
      -> (poller/fork (lambda ()
                        (try
                         (fetch url opts)
                         except
                         (:OSError n) -> (die (format "error: " (strerror n)))
                         (:X509/Invalid why) -> (die why))))
      _ -> (usage))
    (poller/wait-and-schedule)))

(main)
