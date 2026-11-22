;; -*- Mode: Irken -*-

;; look at a mailbox with IMAP (doom/mail/imap.scm).
;;
;; $ IMAP_PASSWORD=... demo/mail/imapls -host mail.example.com -user sam
;;     (each mailbox, with its message counts)
;; $ IMAP_PASSWORD=... demo/mail/imapls -host mail.example.com -user sam INBOX 20
;;     (the last 20 messages in INBOX: date, sender, subject)
;;
;; security: STARTTLS on port 143 by default; -tls for TLS from the
;;  start (port 993), -plain for neither.  with -plain we won't send a
;;  password.

(require "lib/basis.scm")
(require "lib/getopt.scm")
(require "lib/os.scm")
(require "doom/mail/imap.scm")

(define program-options
  (makeopt
   (flag 'k)
   (flag 'tls)
   (flag 'plain)
   (arg 'host (string ""))
   (arg 'port (int 0))
   (arg 'ip (string ""))
   (arg 'root (string ""))
   (arg 'user (string ""))
   (flag 'h)
   (pos* 'args (string ""))
   ))

(define (usage)
  (printf "look at a mailbox with IMAP.\n\n"
          "Usage: " sys.argv[0] " [options] -host <host> -user <name> [<mailbox> [<n>]]\n"
          "  with no mailbox: each mailbox, with its message counts.\n"
          "  with a mailbox: its last <n> (default 10) messages.\n"
          "  [-port <n>]     default 143 (993 with -tls)\n"
          "  [-tls]          TLS from the start, instead of STARTTLS\n"
          "  [-plain]        no TLS at all\n"
          "  [-root <pem>]   trust these certificates instead of the system's roots\n"
          "  [-k]            don't check the server's certificate (insecure!)\n"
          "  [-ip <addr>]    connect to this IPv4 address instead of looking up the host\n"
          "  [-h]            print this usage.\n"
          "the password is in $IMAP_PASSWORD.\n")
  (%exit #f 2))

(define (die msg)
  (printf "imapls: " msg "\n")
  (%exit #f 1))

;; "Subject: hello\r\n" -> "hello", for a header field in <headers>.
(define (header-field headers name)
  (let loop ((lines (string-split headers #\newline)))
    (match lines with
      () -> ""
      (line . tl)
      -> (let ((line (string-trim-right line)))
           (if (starts-with (downcase line) (format (downcase name) ":"))
               (string-trim-both (substring line (+ 1 (string-length name)) (string-length line)))
               (loop tl))))))

(define (list-all s)
  (for-list mb (s.list "" "*")
    (if (member? "\\Noselect" mb.flags string=?)
        (printf mb.name "\n")
        (let ((counts (s.status mb.name (list "MESSAGES" "UNSEEN"))))
          (printf (rpad 30 mb.name)
                  (join (lambda (p) (match p with (:tuple k n) -> (format (downcase k) " " (int n)))) ", " counts)
                  "\n")))))

(define (show-last s mailbox n)
  (let ((sel (s.examine mailbox))
        (uids (s.search "ALL"))
        (count (length uids))
        (last-n (if (> count n) (drop uids (- count n)) uids)))
    (printf mailbox ": " (int sel.exists) " messages\n")
    (when (not (null? last-n))
      (for-closing m (s.fetch last-n "(UID FLAGS BODY.PEEK[HEADER.FIELDS (DATE FROM SUBJECT)])")
        (let ((h (imap/item-string m.items "BODY[HEADER.FIELDS (DATE FROM SUBJECT)]"))
              (flags (match (imap/item m.items "FLAGS") with (maybe:yes v) -> (imap/atoms v) _ -> (list:nil))))
          (printf (lpad 6 (int m.uid)) " "
                  (if (member? "\\Seen" flags string=?) " " "*") " "
                  (rpad 31 (header-field h "Date")) " "
                  (rpad 30 (header-field h "From")) " "
                  (header-field h "Subject") "\n"))))))

(define (run opts)
  (define (flag name) (match (get-bool-opt opts name) with (maybe:yes v) -> v _ -> #f))
  (define (str name) (match (get-string-opt opts name) with (maybe:yes v) -> v _ -> ""))
  (let ((security (cond ((flag 'plain) (imap-security:none))
                        ((flag 'tls) (imap-security:implicit))
                        (else (imap-security:starttls))))
        (port (match (get-int-opt opts 'port) with
                (maybe:yes n) -> (if (> n 0) n (if (eq? security (imap-security:implicit)) 993 143))
                _ -> 143))
        (host (str 'host))
        (user (str 'user))
        (args (match (getopt opts 'args) with
                (maybe:yes (optval:list l)) -> (map (lambda (v) (match v with (optval:string s) -> s _ -> "")) l)
                _ -> (list:nil)))
        (verify (if (flag 'k)
                    (lambda (ders) (parse-x509 (first ders)))
                    (net/check-cert (if (flag 'plain) (list:nil) (net/load-trust-anchors (str 'root))) host))))
    (when (or (= 0 (string-length host)) (= 0 (string-length user)))
      (usage))
    (when (eq? security (imap-security:none))
      (die "won't send a password without TLS"))
    (let ((s (imap/connect host port (str 'ip) security verify)))
      (s.login user (getenv "IMAP_PASSWORD"))
      (match args with
        () -> (list-all s)
        (mailbox) -> (show-last s mailbox 10)
        (mailbox n) -> (show-last s mailbox (string->int n))
        _ -> (usage))
      (s.logout)
      (%exit #f 0))))

;; report failures in a word, not a backtrace.
(define (run* opts)
  (try
   (run opts)
   except
   (:IMAP/No what text)    -> (die (format what ": " text))
   (:IMAP/Bad what text)   -> (die (format what ": (BAD) " text))
   (:IMAP/Bye text)        -> (die (format "server closed: " text))
   (:IMAP/Protocol why)    -> (die (format "protocol: " why))
   (:IMAP/NoTLS)           -> (die "the server doesn't offer STARTTLS")
   (:IMAP/BadArgument s)   -> (die (format "can't send " (string s)))
   (:Reader/Closed why)    -> (die why)
   (:TLS/ClientFailed why) -> (die (format "TLS: " why))
   (:Net/Failed why)       -> (die why)
   (:OSError n)            -> (die (format "error: " (strerror n)))
   (:X509/Invalid why)     -> (die why)
   (:SIG/NoPEMFound path)  -> (die (format "no certificates in " path))))

(define (main)
  (set-verbose-gc #f)
  (let ((opts (try
               (process-options program-options (rest (vector->list sys.argv)))
               except
               (:Getopt/MissingArg _ _) -> (usage)
               (:Getopt/UnknownArg _ _) -> (usage))))
    (when-maybe v (get-bool-opt opts 'h) (usage))
    (poller/fork (lambda () (run* opts)))
    (poller/wait-and-schedule)))

(main)
