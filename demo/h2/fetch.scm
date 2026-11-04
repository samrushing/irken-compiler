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
(require "doom/http/client.scm")

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

(define (print-headers resp)
  (printf (bold "status " resp.status) "\n")
  (for-list h resp.headers
    (match h with
      (:tuple name val) -> (printf name ": " val "\n")))
  (printf "\n"))

(define (fetch url opts)
  (let ((u (url/parse url))
        (verbose? (match (get-bool-opt opts 'v) with (maybe:yes v) -> v _ -> #f))
        (headers? (match (get-bool-opt opts 'i) with (maybe:yes v) -> v _ -> #f))
        (insecure? (match (get-bool-opt opts 'k) with (maybe:yes v) -> v _ -> #f))
        (root (match (get-string-opt opts 'root) with (maybe:yes v) -> v _ -> ""))
        (ip (match (get-string-opt opts 'ip) with
              (maybe:yes v) -> (if (> (string-length v) 0) v (resolve-host u.host))
              _ -> (resolve-host u.host)))
        (anchors (if insecure? (list:nil) (load-trust-anchors root)))
        (verify (if insecure?
                    (lambda (ders)
                      (printf (ansi (bold red) "warning:") " not checking the server's certificate\n")
                      (parse-x509 (first ders)))
                    (https/check-cert anchors u.host))))
    (when verbose?
      (printf "* connecting to " ip ":" (int u.port)
              (if insecure? "" (format " (" (int (length anchors)) " trusted roots)")) "\n"))
    (let ((conn (https/connect u.host u.port ip verify))
          (info conn.info))
      (when verbose?
        (printf "* TLS 1.3 " (sym info.suite) ", " (sym info.group) ", " info.sigalg "\n")
        (for-range i (length info.certs)
          (let ((x (parse-x509 (nth info.certs i))))
            (printf "*   [" (int i) "] " (if (null? x.cert.subject)
                                                (x509-label x.cert)
                                                (x509-name-repr x.cert.subject)) "\n"
                    "*       issued by " (x509-name-repr x.cert.issuer) "\n")))
        (printf "* " (if conn.http2 "HTTP/2" "HTTP/1.1") "\n")
        (flush)
        #u)
      (let ((resp (conn.request "GET" u.path
                                (list (:tuple "user-agent" "irken-fetch/0.1")
                                      (:tuple "accept" "*/*")
                                      (:tuple "accept-encoding" "gzip, deflate"))
                                "")))
        (when (or verbose? headers?)
          (print-headers resp))
        (print-string (http/decode-body resp))
        (conn.close)
        (%exit #f (if (eq? (string-ref resp.status 0) #\2) 0 1))))))

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
                         (:HTTPS/Failed why) -> (die why)
                         (:OSError n) -> (die (format "error: " (strerror n)))
                         (:X509/Invalid why) -> (die why))))
      _ -> (usage))
    (poller/wait-and-schedule)))

(main)
