;; -*- Mode: Irken -*-

;; SMTP client (rfc5321), mainly for submitting mail (rfc6409).
;;
;; supported: EHLO (falling back to HELO), STARTTLS (rfc3207), AUTH
;;  PLAIN and LOGIN (rfc4954), SIZE (rfc1870), 8BITMIME (rfc6152).
;; not yet: PIPELINING, SMTPUTF8, CHUNKING, DSN, AUTH SCRAM.
;;
;; smtp/client works over any {send recv close} (see doom/stream); to
;;  connect, look up the host, and do TLS, use smtp/connect.
;;
;; errors:
;;   (:SMTP/Error what code text)  the server refused <what> ("MAIL", "RCPT"...)
;;   (:SMTP/BadReply line)         the server's reply makes no sense
;;   (:SMTP/BadArgument s)         <s> can't go in a command (e.g. it holds CR/LF)
;;   (:SMTP/NoTLS)                 STARTTLS was wanted, but isn't offered
;;   (:Reader/Closed what)         the server hung up

(require "lib/codecs/base64.scm")
(require "doom/doom.scm")
(require "doom/reader.scm")
(require "doom/net.scm")
(require "doom/tls/client.scm")

(datatype smtp-security
  (:none)      ;; plain text (port 25, or a local relay)
  (:starttls)  ;; plain text, upgraded to TLS before anything else (port 587)
  (:implicit)  ;; TLS from the start (port 465, rfc8314)
  )

(define (smtp/digit? ch)
  (let ((n (char->int ch)))
    (and (>= n 48) (<= n 57))))

;; can <s> go in a command line?
(define (smtp/check-arg s)
  (for-string ch s
    (when (or (eq? ch #\return) (eq? ch #\newline) (= 0 (char->int ch)))
      (raise (:SMTP/BadArgument s))))
  s)

(define (smtp/has-8bit? s)
  (let loop ((i 0))
    (cond ((= i (string-length s)) #f)
          ((> (char->int (string-ref s i)) 127) #t)
          (else (loop (+ i 1))))))

;; the DATA form of a message: CRLF line endings, lines starting with
;;  "." doubled (rfc5321 4.5.2), and the final ".".  <msg> may use LF
;;  or CRLF.
(define (smtp/dot-stuff msg)
  (let ((lines (string-split msg #\newline))
        ;; a final newline leaves an empty last "line": drop it.
        (lines (if (and (not (null? lines)) (= 0 (string-length (last lines))))
                   (butlast lines)
                   lines))
        (parts (list:nil)))
    (for-list line lines
      (let ((n (string-length line))
            (line (if (and (> n 0) (eq? (string-ref line (- n 1)) #\return))
                      (substring line 0 (- n 1))
                      line)))
        (when (and (> (string-length line) 0) (eq? (string-ref line 0) #\.))
          (push! parts "."))
        (push! parts line)
        (push! parts "\r\n")))
    (push! parts ".\r\n")
    (string-concat (reverse parts))))

;; a session over <conn>, which must be at the start of the
;;  conversation (the server's greeting not yet read).  <helo-name>
;;  is our own name, for EHLO; <tls?> says whether <conn> is already
;;  TLS (port 465).
;;
;; returns:
;;   {ehlo starttls auth send-mail command quit extension extensions secure greeting}
(define (smtp/client conn helo-name tls?)

  (let ((conn conn)
        (r (reader/make conn))
        (exts (list:nil))          ;; from EHLO: (:tuple KEYWORD params)
        (tls? tls?)
        (greeting ""))

    ;; --- replies ---

    ;; "250-first" "250-second" "250 last" -> {code=250 lines=(first second last)}
    (define (read-reply)
      (let loop ((lines (list:nil))
                 (code0 -1))
        (let ((line (r.read-line))
              (n (string-length line)))
          (when (or (< n 3)
                    (not (smtp/digit? (string-ref line 0)))
                    (not (smtp/digit? (string-ref line 1)))
                    (not (smtp/digit? (string-ref line 2)))
                    (and (> n 3)
                         (not (eq? (string-ref line 3) #\space))
                         (not (eq? (string-ref line 3) #\-))))
            (raise (:SMTP/BadReply line)))
          (let ((code (string->int (substring line 0 3)))
                (text (if (> n 4) (substring line 4 n) ""))
                (more? (and (> n 3) (eq? (string-ref line 3) #\-))))
            (when (and (>= code0 0) (not (= code code0)))
              (raise (:SMTP/BadReply line)))
            (if more?
                (loop (list:cons text lines) code)
                {code=code lines=(reverse (list:cons text lines))})))))

    (define (reply-text rep)
      (string-join rep.lines " / "))

    (define (positive? rep)
      (= 2 (/ rep.code 100)))

    (define (expect what rep code)
      (when (not (= rep.code code))
        (raise (:SMTP/Error what rep.code (reply-text rep))))
      rep)

    (define (expect-2xx what rep)
      (when (not (positive? rep))
        (raise (:SMTP/Error what rep.code (reply-text rep))))
      rep)

    ;; --- commands ---

    (define (send-line line)
      (conn.send (string-append (smtp/check-arg line) "\r\n")))

    (define (command line)
      (send-line line)
      (read-reply))

    ;; --- extensions ---

    (define (parse-ext line)
      (let ((sp (string-find " " line)))
        (if (= sp -1)
            (:tuple (upcase line) "")
            (:tuple (upcase (substring line 0 sp))
                    (string-trim-both (substring line (+ sp 1) (string-length line)))))))

    ;; the parameters of extension <name> (e.g. "AUTH" -> "PLAIN LOGIN"),
    ;;  if the server offered it.
    (define (extension name)
      (let loop ((l exts))
        (match l with
          () -> (maybe:no)
          ((:tuple k v) . tl) -> (if (string=? k name) (maybe:yes v) (loop tl))
          )))

    (define (has? name)
      (match (extension name) with
        (maybe:yes _) -> #t
        (maybe:no)    -> #f))

    (define (ehlo)
      (when (= 0 (string-length helo-name))
        (raise (:SMTP/BadArgument "an empty name for EHLO")))
      (let ((rep (command (format "EHLO " helo-name))))
        (if (positive? rep)
            ;; the first line is the server's name.
            (set! exts (map parse-ext (rest rep.lines)))
            (begin
              (expect-2xx "HELO" (command (format "HELO " helo-name)))
              (set! exts (list:nil))))))

    ;; <upgrade> gets no arguments, and returns the TLS connection:
    ;;  over the same socket that <conn> uses.
    (define (starttls upgrade)
      (when (not (has? "STARTTLS"))
        (raise (:SMTP/NoTLS)))
      (expect "STARTTLS" (command "STARTTLS") 220)
      ;; anything already buffered came before the handshake, unprotected
      ;;  (cf. CVE-2011-0411).
      (when (> (r.ready) 0)
        (raise (:SMTP/BadReply "data received after the STARTTLS reply")))
      (set! conn (upgrade))
      (set! r (reader/make conn))
      (set! tls? #t)
      ;; what we learned before TLS can't be trusted (rfc3207 4.2).
      (ehlo))

    ;; --- authentication ---

    (define (auth-plain user pass)
      (let ((token (b64-encode (format "\x00" user "\x00" pass)))
            (rep (command (format "AUTH PLAIN " token))))
        ;; a server may still ask, with an empty challenge.
        (expect "AUTH" (if (= rep.code 334) (command token) rep) 235)))

    (define (auth-login user pass)
      (expect "AUTH" (command "AUTH LOGIN") 334)
      (expect "AUTH" (command (b64-encode user)) 334)
      (expect "AUTH" (command (b64-encode pass)) 235))

    ;; with the best mechanism the server offers.
    (define (auth user pass)
      (let ((mechs (match (extension "AUTH") with
                     (maybe:yes v) -> (map upcase (string-split v #\space))
                     (maybe:no)    -> (list:nil))))
        (cond ((member? "PLAIN" mechs string=?) (auth-plain user pass))
              ((member? "LOGIN" mechs string=?) (auth-login user pass))
              ((null? mechs) (raise (:SMTP/Error "AUTH" 0 "server doesn't offer AUTH")))
              (else (raise (:SMTP/Error "AUTH" 0 (format "no mechanism we know in: " (join " " mechs)))))
              )
        #u))

    ;; --- sending ---

    ;; send <msg> (a complete rfc5322 message: headers, blank line,
    ;;  body) from <from> to each of <rcpts>.  returns the recipients
    ;;  the server refused, as (:tuple address code text).  if it
    ;;  refuses them all, raises SMTP/Error "RCPT".
    (define (send-mail from rcpts msg)
      (let ((data (smtp/dot-stuff msg))
            (size (string-length data))
            (eight? (smtp/has-8bit? msg))
            (params (list:nil))
            (refused (list:nil))
            (last-refusal {code=0 lines=(list:nil)}))
        (when (null? rcpts)
          (raise (:SMTP/BadArgument "no recipients")))
        ;; SIZE=0 means no limit.
        (match (extension "SIZE") with
          (maybe:yes limit)
          -> (begin
               (when (and (> (string-length limit) 0)
                          (smtp/digit? (string-ref limit 0))
                          (> (string->int limit) 0)
                          (> size (string->int limit)))
                 (raise (:SMTP/Error "MAIL" 552 (format "message is " (int size) " bytes, server limit is " limit))))
               (push! params (format " SIZE=" (int size))))
          (maybe:no) -> #u)
        ;; without 8BITMIME we send 8-bit data anyway: nearly every server
        ;;  takes it, and refusing would help no one.
        (when (and eight? (has? "8BITMIME"))
          (push! params " BODY=8BITMIME"))
        (expect-2xx "MAIL" (command (format "MAIL FROM:<" from ">" (join "" (reverse params)))))
        (for-list rcpt rcpts
          (let ((rep (command (format "RCPT TO:<" rcpt ">"))))
            (when (not (positive? rep))
              (set! last-refusal rep)
              (push! refused (:tuple rcpt rep.code (reply-text rep))))))
        (when (= (length refused) (length rcpts))
          (command "RSET")
          (raise (:SMTP/Error "RCPT" last-refusal.code (reply-text last-refusal))))
        (expect "DATA" (command "DATA") 354)
        (conn.send data)
        (expect-2xx "DATA" (read-reply))
        (reverse refused)))

    ;; polite, but errors here don't matter: the mail has gone.
    (define (quit)
      (try
       (begin (command "QUIT") #u)
       except
       (:Reader/Closed _) -> #u
       (:SMTP/BadReply _) -> #u)
      (conn.close))

    ;; --- body of smtp/client ---
    (let ((rep (read-reply)))
      (expect "greeting" rep 220)
      (set! greeting (reply-text rep)))

    {ehlo       = ehlo
     starttls   = starttls
     auth       = auth
     send-mail  = send-mail
     command    = command
     quit       = quit
     extension  = extension
     extensions = (lambda () exts)
     secure     = (lambda () tls?)
     greeting   = (lambda () greeting)
     }
    ))

;; connect to <host>:<port> (at <ip>, if not ""), with <security>, and
;;  say EHLO.  <verify> checks the server's certificates (see
;;  net/check-cert); it isn't used with (smtp-security:none).
(define (smtp/connect host port ip security verify helo-name)
  (let ((ip (if (= 0 (string-length ip)) (net/resolve-host host) ip))
        (sock (doom/make (tcp4-sock))))
    (define (tls)
      (doom/stream (tls13/client sock host (list:nil) verify)))
    (sock.connect (address/make4 ip port))
    (let ((implicit? (eq? security (smtp-security:implicit)))
          (s (smtp/client (if implicit? (tls) (doom/stream sock)) helo-name implicit?)))
      (s.ehlo)
      (match security with
        (smtp-security:starttls) -> (s.starttls tls)
        _ -> #u)
      s)))
