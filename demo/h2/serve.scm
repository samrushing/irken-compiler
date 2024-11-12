;; -*- Mode: Irken -*-

;;
;; goal: a bog-standard GET handler with file listing for a directory.
;;
;; source: stripped-down from the demo/maze handler.
;;
;; question: can we list a directory?
;;      yes: os/listdir.  gives type/name, no stat data.

(require "lib/basis.scm")
(require "lib/getopt.scm")
(require "lib/random.scm")
(require "lib/mtwist.scm")
(require "lib/posix.scm")
(require "lib/regex.scm")

(require "doom/tls/tls13.scm")
(require "doom/socket.scm")
(require "doom/http/h2.scm")
(require "doom/http/html.scm")

(require "demo/h2/redirect.scm")
(require "demo/h2/stats.scm")
(require "demo/h2/handler.scm")
(require "demo/h2/file-handler.scm")
(require "demo/h2/timestamp.scm")
(require "demo/h2/dns.scm") ;; for ptr lookups

(define (send-status request code)
  (debugf "=> " (ansi red code) " " (string (if-maybe p (lookup-header ":path" request.headers) p "none")) "\n")
  (request.send-headers code (list:nil) #f))

(define (send-404 request)
  (send-status request "404"))

(define (note-event stat event)
  (push! stat.events event)
  (match event with
    (conn-event:tls-error (maybe:yes desc) s)
    -> (debugf "tls-error " (sym (tls-alert-desc->name desc)) " " (string s) "\n")
    (conn-event:tls-error (maybe:no) s)
    -> (debugf "tls-error " (string s) "\n")
    (conn-event:h2-error err)
    -> (debugf "h2-error " (string err) "\n")
    (conn-event:os-error err)
    -> (debugf "os-error " (string err) "\n")
    _
    -> #u
    ))

(define (h2-channel h2-handler x509s skey alpns)
  (lambda (addr sockfun)
    (poller/fork
     (lambda ()
       (let ((stat (*stats*.add addr)))
         (try
          (let ((sock (tls13/make-channel sockfun x509s skey alpns)))
            (note-event stat (conn-event:tls-good (sock.get-info)))
            (try
             ;; create and invoke h2 read-frames loop.
             ((make-h2-conn addr sock h2-handler))
             except
             (:Doom/EOF _)
             -> (note-event stat (conn-event:os-error "eof"))
             (:H2/BadFrame f)
             -> (note-event stat (conn-event:h2-error "bad-frame"))
             (:H2/BadPreface p)
             -> (note-event stat (conn-event:h2-error "bad-preface"))
             (:TLS/Alert kind what)
             -> (note-event stat (conn-event:tls-error (maybe:yes kind) what))
             (:TLS/Failed (maybe:yes desc))
             -> (note-event stat (conn-event:tls-error (maybe:yes desc) ""))
             (:TLS/Failed (maybe:no))
             -> (note-event stat (conn-event:tls-error (maybe:no) "fail/1"))
             (:OSError n)
             -> (note-event stat (conn-event:os-error (strerror n)))
             )
            (sock.set-closed)
            (sock.close))
          except
          (:TLS/Failed mdesc)
          -> (note-event stat (conn-event:tls-error mdesc "fail"))
          (:OSError n)
          -> (note-event stat (conn-event:os-error (strerror n)))
          )
         )))))

;; handler := (lambda (sockfun) ...)
(define (serve ip port handler)
  (let ((sock (doom/make (tcp4-sock)))
        (addr (address/make4 ip port)))
    (sock.bind addr)
    (sock.listen 5)
    (debugf "starting h2s server... on " (address/unparse addr) "\n")
    (debugf "  server fd = " (int (sock.get-fd)) "\n")
    (let loop ()
      (debugf "accept wait...\n")
      (try
       (let (((sockfun addr) (sock.accept))
             (addr-string (address/unparse addr)))
         (debugf "client: addr " addr-string "\n")
         (handler addr-string sockfun)
         (loop))
       except (:OSError errno)
       -> (if (= errno ECONNABORTED)
              (loop)
              (debugf "OSError on accepting socket: " (int errno) "\n"))
       )
      )
    (debugf "after accept loop.\n")
    ))

(define (read-certs path)
  (with-file file (file/open-read path)
    (let ((pgen (pem-read-gen (file-char-generator file)))
          (result '()))
      (for pem pgen
        (match pem with
          (:tuple "CERTIFICATE" asn1) -> (push! result asn1)
          _ -> (raise (:TLS/ExpectedCertificate path))
          ))
      (reverse result))))

(define *logfile* (maybe:no))
(define *stats* (make-server-history 1000))
(define *expires-delta* 3600) ;; for the Expires: header - one hour.

(define program-options
  (makeopt
   (arg 'root (string "/var/local/www/data"))
   (arg 'cert (string "cert.pem"))
   (arg 'key  (string "key.pem"))
   (arg 'log  (string "/var/log/irken-httpd.log"))
   (flag 'r)
   (pos 'addr (string "127.0.0.1:4433"))
   (flag 'h)
   ))

(define (get-options)
  (if (< sys.argc 2)
      (usage)
      (try
       (process-options program-options (rest (vector->list sys.argv)))
       except
       (:Getopt/MissingArg _ _) -> (usage)
       (:Getopt/UnknownArg _ _) -> (usage)
       )))

(define (usage)
  (printf "HTTP/2 TLS-1.3 file server.\n\n")
  (printf "Usage: " sys.argv[0] " <addr>\n"
          "  [-root = /var/local/www/data\n"
          "  [-cert = cert.pem]\n"
          "  [-key = key.pem]\n"
          "  [-log = /var/log/irken-httpd.log]\n"
          "  [-r] disable redirect (301 Moved Permanently) on port 80\n"
          "  [-h] print this usage.\n"
          "\n"
          " note: the root filesystem is expected to be of the form:\n"
          "      /var/www/root/hostname0/index.html\n"
          "      /var/www/root/hostname1/index.html\n"
          "   where each <hostname> is the FQDN/authority for that site.\n"
          "\n"
          "example: $ " sys.argv[0] " 127.0.0.1:4433 -root /var/www/root/\n"
          "\n"
          )
  (%exit #f -1))

(define (split-addr addr)
  (match (string-split addr #\:) with
    (ip port) -> (:tuple ip (string->int port))
    (ip)      -> (:tuple ip 443)
    _         -> (usage)
    ))

;; placeholder
(define (*dnscache* addr)
  "")

(define *start-time* (gettime/realtime))

(define (main)
  (let ((opts (get-options))
        (addr "127.0.0.1:4433")
        (root "/var/local/www/data")
        (cert "cert.pem")
        (key  "key.pem")
        (redirect? #t)
        (log  "/var/log/irken-httpd.log")
        (usermap (tree/make string-cmp ("foo" "bar") ("gandalf" "shadowfax"))))
    (when-maybe v (get-bool-opt opts 'h) (usage))
    (when-maybe v (get-bool-opt opts 'r) (set! redirect? #f))
    (when-maybe v (get-string-opt opts 'root) (set! root v))
    (when-maybe v (get-string-opt opts 'cert) (set! cert v))
    (when-maybe v (get-string-opt opts 'key)  (set! key v))
    (when-maybe v (get-string-opt opts 'log)  (set! log v))
    (when-maybe v (get-string-opt opts 'addr) (set! addr v))
    (set! *dnscache* (dnscache/make "127.0.0.1" 1000))

    (let ((x509s (read-certs cert))
          (skey (read-skey key))
          ((ip port) (split-addr addr))
          (handler (h2-handler
                    (multi-handler
                     (list (basic-auth-wrapper usermap "status" "/status" (status-handler addr opts))
                           favicon-handler
                           (host-handler root)))))
          ;;(handler (h2-handler (host-handler root)))
          )
      (set! *logfile* (maybe:yes (file/open-append log #o644)))
      (debugf "read " (int (length x509s)) " certs.\n")
      (debugf "redirect? " (bool redirect?) "\n")
      (when redirect?
        (poller/fork (lambda () (serve-redirect ip 80))))
      (serve ip port (h2-channel handler x509s skey '("h2")))
      (poller/wait-and-schedule))))

;; this helps with localhost testing - firefox will race its own cache
;;   and RST_STREAMs etc resulting in unwanted SIGPIPE.
;;(signal/ignore SIGPIPE)
(main)
(printf "after main.\n")
