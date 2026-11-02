;; -*- Mode: Irken -*-

;; HTTPS requests: TLS 1.3 (doom/tls/client.scm), then HTTP/2 or
;;  HTTP/1.1 as the server chooses with ALPN.
;;
;; errors are reported as (:HTTPS/Failed reason).

(require "lib/posix.scm")
(require "lib/time.scm")
(require "lib/net/dns.scm")
(require "lib/codecs/zlib.scm")
(require "doom/doom.scm")
(require "doom/tls/client.scm")
(require "doom/http/h2client.scm")
(require "doom/http/h1client.scm")

;; https://host[:port][/path] -> {host port path authority}
(define (url/parse url)
  (if (not (starts-with url "https://"))
      (raise (:HTTPS/Failed (format "not an https:// URL: " url)))
      (let ((rest (substring url 8 (string-length url)))
            (slash (string-find "/" rest))
            (hostport (if (= slash -1) rest (substring rest 0 slash)))
            (path (if (= slash -1) "/" (substring rest slash (string-length rest)))))
        (match (string-split hostport #\:) with
          (host)      -> {host=host port=443 path=path authority=host}
          (host port) -> {host=host port=(string->int port) path=path authority=hostport}
          _           -> (raise (:HTTPS/Failed (format "bad URL: " url)))
          ))))

;; --- names ---

;; whitespace-separated fields.
(define (fields s)
  (let ((r (list:nil))
        (cur (list:nil)))
    (define (end-field)
      (when (not (null? cur))
        (push! r (list->string (reverse cur)))
        (set! cur (list:nil))))
    (for-string ch s
      (if (or (char=? ch #\space) (char=? ch #\tab) (char=? ch #\return) (char=? ch #\newline))
          (end-field)
          (push! cur ch)))
    (end-field)
    (reverse r)))

;; the fields of each line of a file like /etc/hosts, without comments.
(define (config-lines path)
  (if (isfile? path)
      (with-file f (file/open-read path)
        (let ((r (list:nil)))
          (for-list line (file/read-lines f)
            (push! r (fields (first (string-split line #\#)))))
          (reverse r)))
      (list:nil)))

(define (hosts-lookup host)
  (let loop ((lines (config-lines "/etc/hosts")))
    (match lines with
      () -> (maybe:no)
      ((ip . names) . rest)
      -> (if (and (ipv4-literal? ip) (member? host names string=?))
             (maybe:yes ip)
             (loop rest))
      (_ . rest) -> (loop rest)
      )))

;; the first IPv4 nameserver in /etc/resolv.conf
(define (find-nameserver)
  (let loop ((lines (config-lines "/etc/resolv.conf")))
    (match lines with
      () -> (raise (:HTTPS/Failed "no IPv4 nameserver in /etc/resolv.conf"))
      (("nameserver" ip) . rest) -> (if (ipv4-literal? ip) ip (loop rest))
      (_ . rest) -> (loop rest)
      )))

;; look up an A record (following CNAMEs is the resolver's job).
(define (dns-lookup host)
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
      r)))

;; host -> IPv4 address: a literal, /etc/hosts, or DNS.
(define (resolve-host host)
  (if (ipv4-literal? host)
      host
      (match (hosts-lookup host) with
        (maybe:yes ip) -> ip
        (maybe:no)
        -> (match (dns-lookup host) with
             (maybe:yes ip) -> ip
             (maybe:no) -> (raise (:HTTPS/Failed (format "can't find an address for " host)))
             ))))

;; --- trust ---

;; from <path>, or (if "") the system's bundle.
(define (load-trust-anchors path)
  (if (> (string-length path) 0)
      (x509-load-anchors path)
      (let loop ((paths x509-system-anchor-paths))
        (match paths with
          () -> (raise (:HTTPS/Failed "can't find the system's trusted roots"))
          (p . rest) -> (if (isfile? p) (x509-load-anchors p) (loop rest))))))

;; --- content-encoding ---

;; does <s> start with a zlib (rfc1950) header?
(define (zlib-header? s)
  (and (>= (string-length s) 2)
       (let ((b0 (char->int (string-ref s 0)))
             (b1 (char->int (string-ref s 1))))
         (and (= 8 (logand b0 #x0f))                    ;; CM = deflate
              (= 0 (mod (+ (* b0 256) b1) 31))))))      ;; FCHECK

(define (inflate-string s window-bits)
  (string-concat (generator->list (inflate2 (list-generator (list s)) 16384 window-bits))))

;; undo any content-encoding.  "deflate" is supposed to mean
;;  zlib-wrapped (rfc9110 8.4.1.2), but some servers send raw deflate.
(define (http/decode-body resp)
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
   (:Zlib/APIFailure _) -> (raise (:HTTPS/Failed "can't decompress the response body"))))

;; --- connections ---

;; connect to <host>:<port> (at <ip>, if not ""), and handshake.
;;  <verify> is given the server's certificates; see tls13/client.
;;  returns {tls info http2 request close}; request takes
;;  (method path headers body).  an HTTP/1.1 connection takes only one
;;  request.
(define (https/connect host port ip verify)
  (let ((ip (if (= 0 (string-length ip)) (resolve-host host) ip))
        (sock (doom/make (tcp4-sock)))
        (_ (sock.connect (address/make4 ip port)))
        (tls (try
              (tls13/client sock host '("h2" "http/1.1") verify)
              except
              (:TLS/ClientFailed why) -> (raise (:HTTPS/Failed (format "TLS: " why)))))
        (info (tls.get-info))
        (h2? (match info.alpn with
               (maybe:yes "h2") -> #t
               _ -> #f)) ;; "http/1.1", or no ALPN at all
        (h2 (if h2? (maybe:yes (h2/client tls)) (maybe:no)))
        (authority (if (= port 443) host (format host ":" (int port)))))
    (define (request method path headers body)
      (try
       (match h2 with
         (maybe:yes c) -> (c.request method authority path headers body)
         (maybe:no)    -> (h1/request tls method authority path headers body))
       except
       (:H2/GoAway code debug)
       -> (raise (:HTTPS/Failed (format "HTTP/2: server sent GOAWAY " (h2-error-name code) " " debug)))
       (:H2/StreamReset code)
       -> (raise (:HTTPS/Failed (format "HTTP/2: server reset the stream: " (h2-error-name code))))
       (:H2/BadFrame what)
       -> (raise (:HTTPS/Failed (format "HTTP/2: bad frame: " what)))
       (:H2/BodyTooLarge n)
       -> (raise (:HTTPS/Failed (format "HTTP/2: request body too large: " (int n))))
       (:H1/BadResponse what)
       -> (raise (:HTTPS/Failed (format "HTTP/1.1: " what)))
       (:Doom/EOF _)
       -> (raise (:HTTPS/Failed "connection closed by server"))))
    (define (close) (tls.close))
    {tls=tls info=info http2=h2? request=request close=close}
    ))

(define (https/check-cert anchors host)
  (let ((now (let ((t (gettime/realtime))) t.sec)))
    (lambda (ders) (x509-verify-server ders anchors host now))))

;; one request on its own connection.  returns {status headers body},
;;  with the body decoded.
(define (https/request method url headers body anchors)
  (let ((u (url/parse url))
        (conn (try
               (https/connect u.host u.port "" (https/check-cert anchors u.host))
               except
               (:OSError n) -> (raise (:HTTPS/Failed (format "connecting to " u.authority ": " (strerror n))))))
        (resp (conn.request method u.path headers body)))
    (conn.close)
    {status=resp.status headers=resp.headers body=(http/decode-body resp)}))
