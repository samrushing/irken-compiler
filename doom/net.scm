;; -*- Mode: Irken -*-

;; things most network clients need: finding a host's IPv4 address,
;;  and checking a server's certificates.
;;
;; errors are reported as (:Net/Failed reason).

(require "lib/posix.scm")
(require "lib/time.scm")
(require "lib/net/dns.scm")
(require "lib/crypto/x509path.scm")
(require "doom/doom.scm")

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
      () -> (raise (:Net/Failed "no IPv4 nameserver in /etc/resolv.conf"))
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
(define (net/resolve-host host)
  (if (ipv4-literal? host)
      host
      (match (hosts-lookup host) with
        (maybe:yes ip) -> ip
        (maybe:no)
        -> (match (dns-lookup host) with
             (maybe:yes ip) -> ip
             (maybe:no) -> (raise (:Net/Failed (format "can't find an address for " host)))
             ))))

;; --- trust ---

;; from <path>, or (if "") the system's bundle.
(define (net/load-trust-anchors path)
  (if (> (string-length path) 0)
      (x509-load-anchors path)
      (let loop ((paths x509-system-anchor-paths))
        (match paths with
          () -> (raise (:Net/Failed "can't find the system's trusted roots"))
          (p . rest) -> (if (isfile? p) (x509-load-anchors p) (loop rest))))))

;; a certificate check for tls13/client: <ders> must chain to one of
;;  <anchors> and name <host>.
(define (net/check-cert anchors host)
  (let ((now (let ((t (gettime/realtime))) t.sec)))
    (lambda (ders) (x509-verify-server ders anchors host now))))
