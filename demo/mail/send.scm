;; -*- Mode: Irken -*-

;; send a message with SMTP (doom/mail/smtp.scm).
;;
;; $ SMTP_PASSWORD=... demo/mail/send -host mail.example.com -user sam \
;;     -from sam@example.com -f message.txt fred@example.com wilma@example.com
;;
;; the message (from -f, or stdin) is a complete rfc5322 message:
;;  headers, a blank line, and the body.  it isn't changed, so it
;;  needs its own Date: and Message-ID: (or the server must add them).
;;
;; security: STARTTLS on port 587 by default; -tls for TLS from the
;;  start (port 465), -plain for neither (port 25).  with -plain we
;;  won't send a password.

(require "lib/basis.scm")
(require "lib/getopt.scm")
(require "lib/os.scm")
(require "doom/mail/smtp.scm")

(define program-options
  (makeopt
   (flag 'v)
   (flag 'k)
   (flag 'tls)
   (flag 'plain)
   (arg 'host (string ""))
   (arg 'port (int 0))
   (arg 'ip (string ""))
   (arg 'root (string ""))
   (arg 'helo (string ""))
   (arg 'user (string ""))
   (arg 'from (string ""))
   (arg 'f (string ""))
   (flag 'h)
   (pos* 'to (string ""))
   ))

(define (usage)
  (printf "send a message with SMTP.\n\n"
          "Usage: " sys.argv[0] " [options] -host <host> -from <addr> <rcpt> ...\n"
          "  [-f <file>]     the message (default: stdin)\n"
          "  [-port <n>]     default 587 (465 with -tls, 25 with -plain)\n"
          "  [-tls]          TLS from the start, instead of STARTTLS\n"
          "  [-plain]        no TLS at all\n"
          "  [-user <name>]  authenticate, with the password in $SMTP_PASSWORD\n"
          "  [-helo <name>]  our name, for EHLO (default: localhost)\n"
          "  [-root <pem>]   trust these certificates instead of the system's roots\n"
          "  [-k]            don't check the server's certificate (insecure!)\n"
          "  [-ip <addr>]    connect to this IPv4 address instead of looking up the host\n"
          "  [-v]            say what's happening\n"
          "  [-h]            print this usage.\n")
  (%exit #f 2))

(define (die msg)
  (printf "send: " msg "\n")
  (%exit #f 1))

(define (read-all f)
  (let loop ((parts (list:nil)))
    (let ((s (file/read-buffer f)))
      (if (= 0 (string-length s))
          (string-concat (reverse parts))
          (loop (list:cons s parts))))))

(define (send opts)
  (define (flag name) (match (get-bool-opt opts name) with (maybe:yes v) -> v _ -> #f))
  (define (str name) (match (get-string-opt opts name) with (maybe:yes v) -> v _ -> ""))
  (define (str-or name default) (let ((v (str name))) (if (= 0 (string-length v)) default v)))
  (let ((verbose? (flag 'v))
        (insecure? (flag 'k))
        (security (cond ((flag 'plain) (smtp-security:none))
                        ((flag 'tls) (smtp-security:implicit))
                        (else (smtp-security:starttls))))
        (port (match (get-int-opt opts 'port) with
                (maybe:yes n) -> (if (> n 0) n
                                     (match security with
                                       (smtp-security:none)     -> 25
                                       (smtp-security:implicit) -> 465
                                       (smtp-security:starttls) -> 587))
                _ -> 587))
        (host (str 'host))
        (user (str 'user))
        (from (str 'from))
        (rcpts (match (getopt opts 'to) with
                 (maybe:yes (optval:list l))
                 -> (map (lambda (v) (match v with (optval:string s) -> s _ -> "")) l)
                 _ -> (list:nil)))
        (msg (if (> (string-length (str 'f)) 0)
                 (read-all (file/open-read (str 'f)))
                 (read-all (file/open-stdin))))
        (verify (if insecure?
                    (lambda (ders)
                      (printf (ansi (bold red) "warning:") " not checking the server's certificate\n")
                      (parse-x509 (first ders)))
                    (net/check-cert (net/load-trust-anchors (str 'root)) host))))
    (when (or (= 0 (string-length host)) (= 0 (string-length from)) (null? rcpts))
      (usage))
    (when (and (> (string-length user) 0) (eq? security (smtp-security:none)))
      (die "won't send a password without TLS"))
    (when verbose?
      (printf "* connecting to " host ":" (int port) "\n"))
    (let ((s (smtp/connect host port (str 'ip) security verify (str-or 'helo "localhost"))))
      (when verbose?
        (printf "* " (s.greeting) "\n")
        (printf "* " (if (s.secure) "TLS" "no TLS") "; extensions:\n")
        (for-list ext (s.extensions)
          (match ext with
            (:tuple k v) -> (printf "*   " k " " v "\n"))))
      (when (> (string-length user) 0)
        (s.auth user (getenv "SMTP_PASSWORD"))
        (when verbose? (printf "* authenticated as " user "\n")))
      (let ((refused (s.send-mail from rcpts msg)))
        (for-list r refused
          (match r with
            (:tuple addr code text) -> (printf "refused: " addr ": " (int code) " " text "\n")))
        (when verbose?
          (printf "* sent to " (int (- (length rcpts) (length refused))) " of " (int (length rcpts)) "\n"))
        (s.quit)
        (%exit #f (if (null? refused) 0 1))))))

(define (main)
  (set-verbose-gc #f)
  (let ((opts (try
               (process-options program-options (rest (vector->list sys.argv)))
               except
               (:Getopt/MissingArg _ _) -> (usage)
               (:Getopt/UnknownArg _ _) -> (usage))))
    (when-maybe v (get-bool-opt opts 'h) (usage))
    (poller/fork
     (lambda ()
       (try
        (send opts)
        except
        (:SMTP/Error what code text) -> (die (format what ": " (int code) " " text))
        (:SMTP/BadReply line)        -> (die (format "bad reply from server: " (string line)))
        (:SMTP/BadArgument s)        -> (die (format "can't send " (string s)))
        (:SMTP/NoTLS)                -> (die "server doesn't offer STARTTLS")
        (:Reader/Closed why)         -> (die why)
        (:Reader/LineTooLong n)      -> (die "reply line too long")
        (:TLS/ClientFailed why)      -> (die (format "TLS: " why))
        (:Net/Failed why)            -> (die why)
        (:OSError n)                 -> (die (format "error: " (strerror n)))
        (:X509/Invalid why)          -> (die why)
        (:SIG/NoPEMFound path)       -> (die (format "no certificates in " path)))))
    (poller/wait-and-schedule)))

(main)
