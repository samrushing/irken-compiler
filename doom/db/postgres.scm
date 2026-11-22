;; -*- Mode: Irken -*-

;; PostgreSQL client: the frontend/backend protocol, version 3.0.
;;  https://www.postgresql.org/docs/current/protocol.html
;;
;; supported:
;;   TLS (SSLRequest, then TLS 1.3 with ALPN "postgresql")
;;   authentication: trust, cleartext password, SCRAM-SHA-256 (rfc7677),
;;     and SCRAM-SHA-256-PLUS over TLS (tls-server-end-point, rfc5929)
;;   the simple query protocol (several statements at once)
;;   the extended query protocol: parameters, prepared statements
;;   cancelling a running query
;; not (yet): md5 passwords, COPY, unix-domain sockets, binary formats.
;;
;; all values travel as text.  a value is (maybe:yes "text"), or
;;  (maybe:no) for NULL.  a result is
;;    {columns=(list {name type}) rows=(list (vector (maybe string))) tag}
;;  where type is the column's type oid, and tag is the command tag
;;  ("SELECT 3", "INSERT 0 1", ...).
;;
;; errors:
;;   (:PG/Error fields)    the server reported an error: see pg/error-field.
;;                         the connection is still usable.
;;   (:PG/AuthFailed why)  we refused the server (e.g. a bad SCRAM signature)
;;   (:PG/Unsupported why) the server wants something we can't do
;;   (:PG/Protocol why)    the server's messages make no sense
;;   (:PG/NoTLS)           TLS was required, but the server won't
;;   (:PG/Busy why)        this thread has a rows stream open
;;   (:Reader/Closed what) the server hung up

(require "lib/codecs/base64.scm")
(require "lib/crypto/pbkdf2.scm")
(require "lib/crypto/x509.scm")
(require "lib/urandom.scm")
(require "doom/doom.scm")
(require "doom/reader.scm")
(require "doom/net.scm")
(require "doom/tls/client.scm")

(datatype pg-tls
  (:disable)   ;; plain text
  (:require)   ;; TLS, or fail
  )

;; --- encoding ---

(define (pg/int32 n)
  (let ((n (logand n #xffffffff)))
    (list->string
     (map (lambda (shift) (int->char (logand #xff (>> n shift))))
          '(24 16 8 0)))))

(define (pg/int16 n)
  (let ((n (logand n #xffff)))
    (list->string (list (int->char (>> n 8)) (int->char (logand n #xff))))))

(define (pg/cstring s)
  (for-string ch s
    (when (= 0 (char->int ch))
      (raise (:PG/BadArgument "a string with a NUL in it"))))
  (string-append s "\x00"))

;; a frontend message: type byte, int32 length (counting itself), body.
(define (pg/msg type body)
  (string-append (char->string type) (pg/int32 (+ 4 (string-length body))) body))

;; --- decoding ---

;; reading the body of one message.
(define (pg/cursor s)
  (let ((pos 0))
    (define (need n)
      (when (> (+ pos n) (string-length s))
        (raise (:PG/Protocol "message too short"))))
    (define (byte)
      (need 1)
      (inc! pos)
      (char->int (string-ref s (- pos 1))))
    (define (int16)
      (let ((hi (byte)) (lo (byte)))
        (let ((n (logior (<< hi 8) lo)))
          (if (>= n #x8000) (- n #x10000) n))))
    (define (int32)
      (let ((hi (int16)) (lo (int16)))
        (logior (<< hi 16) (logand lo #xffff))))
    (define (bytes n)
      (need n)
      (inc! pos n)
      (substring s (- pos n) pos))
    (define (cstring)
      (let ((i (string-find-from "\x00" s pos)))
        (when (= i -1)
          (raise (:PG/Protocol "unterminated string")))
        (let ((r (substring s pos i)))
          (set! pos (+ i 1))
          r)))
    (define (rest)
      (let ((r (substring s pos (string-length s))))
        (set! pos (string-length s))
        r))
    {byte=byte int16=int16 int32=int32 bytes=bytes cstring=cstring rest=rest}
    ))

;; for messages with just one thing in them.
(define (pg/first-int32 body) (let ((c (pg/cursor body))) (c.int32)))
(define (pg/first-byte body) (let ((c (pg/cursor body))) (c.byte)))
(define (pg/first-cstring body) (let ((c (pg/cursor body))) (c.cstring)))

;; ErrorResponse and NoticeResponse: (:tuple code value) pairs, where
;;  code is a field type: #\S severity, #\C SQLSTATE code, #\M message,
;;  #\D detail, #\H hint, #\P position...
(define (pg/parse-fields body)
  (let ((c (pg/cursor body))
        (r (list:nil)))
    (let loop ()
      (let ((code (c.byte)))
        (when (not (= code 0))
          (push! r (:tuple (int->char code) (c.cstring)))
          (loop))))
    (reverse r)))

;; the field <code> of an error or notice, or "".
(define (pg/error-field fields code)
  (let loop ((l fields))
    (match l with
      () -> ""
      ((:tuple k v) . tl) -> (if (eq? k code) v (loop tl))
      )))

;; "ERROR 42P01: relation "x" does not exist"
(define (pg/error-repr fields)
  (format (pg/error-field fields #\S) " " (pg/error-field fields #\C) ": "
          (pg/error-field fields #\M)
          (let ((d (pg/error-field fields #\D)))
            (if (= 0 (string-length d)) "" (format " (" d ")")))))

;; --- SCRAM-SHA-256 (rfc5802, rfc7677) ---

;; "r=abc,s=xyz,i=4096" -> the value of <key>
(define (scram/attr msg key)
  (let loop ((parts (string-split msg #\,)))
    (match parts with
      () -> (raise (:PG/AuthFailed (format "no '" (char key) "' in SCRAM message")))
      (part . tl)
      -> (if (and (>= (string-length part) 2)
                  (eq? (string-ref part 0) key)
                  (eq? (string-ref part 1) #\=))
             (substring part 2 (string-length part))
             (loop tl)))))

(define (scram/hmac key data)
  (let ((h (hmac/sha256.make key)))
    (h.update data)
    (h.final)))

;; (no early exit: when this fails, it says nothing about where.)
(define (scram/same? a b)
  (and (= (string-length a) (string-length b))
       (let ((diff 0))
         (for-range i (string-length a)
           (set! diff (logior diff (logxor (char->int (string-ref a i))
                                           (char->int (string-ref b i))))))
         (= diff 0))))

;; tls-server-end-point channel binding (rfc5929 4.1): a hash of the
;;  server's certificate, with the hash its signature uses (SHA-256 for
;;  MD5 or SHA-1).  (maybe:no) if there is none (e.g. Ed25519).
(define (scram/end-point der)
  (let ((x (parse-x509 der)))
    (match x.algo.oid with
      (1 2 840 113549 1 1 5)  -> (maybe:yes (sha256 der))   ;; sha1WithRSA
      (1 2 840 113549 1 1 11) -> (maybe:yes (sha256 der))
      (1 2 840 113549 1 1 12) -> (maybe:yes (sha384 der))
      (1 2 840 113549 1 1 13) -> (maybe:yes (sha512 der))
      (1 2 840 10045 4 1)     -> (maybe:yes (sha256 der))   ;; ecdsa-with-SHA1
      (1 2 840 10045 4 3 2)   -> (maybe:yes (sha256 der))
      (1 2 840 10045 4 3 3)   -> (maybe:yes (sha384 der))
      (1 2 840 10045 4 3 4)   -> (maybe:yes (sha512 der))
      _                       -> (maybe:no)
      )))

;; --- connections ---

;; run <body> holding <mutex>, returning its value.
(defmacro pg/locked
  (pg/locked m body ...)
  -> (begin
       (m.acquire)
       (try
        (let (($r (begin body ...)))
          (m.release)
          $r)
        except
        EXN
        -> (begin
             (m.release)
             (raise EXN)))))

;; a session over <conn> (a {send recv close}, see doom/stream), which
;;  must be new: we send the startup message.  <cert> is the server's
;;  certificate (DER) if <conn> is TLS, else "": it enables channel
;;  binding.  <cancel> sends a CancelRequest, given the backend's
;;  process id and secret key.
;;
;; returns {query execute prepare rows rows-batch parameter status pid notices close cancel}
;;   (query sql) -> (list result), one per statement
;;   (execute sql params) -> result: one statement, $1, $2... are <params>
;;   (prepare sql) -> {run close}: (run params) -> result
;;   (rows sql params) -> {columns rows}: rows is a closable generator,
;;      fetching 500 at a time; (rows-batch sql params batch) chooses the batch.
;;   (parameter name) -> (maybe string): a server parameter, e.g. "server_version"
;;   (status) -> #\I idle, #\T in a transaction, #\E in a failed transaction
;;   (notices) -> notices and notifications since the last call:
;;      (list (:tuple kind text)), kind "notice" or a NOTIFY channel's name.
(define (pg/client conn user password database cert cancel)

  (let ((r (reader/make conn))
        (params (list:nil))      ;; ParameterStatus: (:tuple name value)
        (pid 0)
        (secret 0)
        (txn #\I)
        (notes (list:nil))
        (mutex (mutex/make))
        (stream-thread -1)       ;; the thread with a rows stream open, or -1
        (counter 0))

    (define (send s) (conn.send s))

    ;; --- messages ---

    ;; the next message, as (:tuple type body).
    (define (read-msg)
      (let ((head (r.read-n 5))
            (c (pg/cursor head))
            (type (int->char (c.byte)))
            (len (c.int32)))
        (when (or (< len 4) (> len #x40000000))
          (raise (:PG/Protocol (format "bad message length " (int len)))))
        (:tuple type (r.read-n (- len 4)))))

    ;; the next message that isn't one the server may send at any time.
    (define (next)
      (match (read-msg) with
        (:tuple #\S body)
        -> (let ((c (pg/cursor body))
                 (name (c.cstring))
                 (value (c.cstring)))
             (set! params (list:cons (:tuple name value)
                                     (filter (lambda (p) (match p with (:tuple n _) -> (not (string=? n name)))) params)))
             (next))
        (:tuple #\N body)
        -> (begin (push! notes (:tuple "notice" (pg/error-repr (pg/parse-fields body)))) (next))
        (:tuple #\A body)
        -> (let ((c (pg/cursor body))
                 (_ (c.int32))
                 (channel (c.cstring))
                 (payload (c.cstring)))
             (push! notes (:tuple channel payload))
             (next))
        msg -> msg
        ))

    (define (unexpected type)
      (raise (:PG/Protocol (format "unexpected message '" (char type) "'"))))

    ;; --- startup and authentication ---

    (define (startup)
      (send
       (let ((body (string-append
                    (pg/int32 196608) ;; protocol 3.0
                    (string-concat
                     (map pg/cstring
                          (list "user" user
                                "database" database
                                "client_encoding" "UTF8"
                                "application_name" "irken")))
                    "\x00")))
         (string-append (pg/int32 (+ 4 (string-length body))) body))))

    (define (sasl-initial mech data)
      (send (pg/msg #\p (string-append (pg/cstring mech) (pg/int32 (string-length data)) data))))

    ;; returns once the server says AuthenticationOk.
    (define (scram mechs)
      (let ((binding (if (and (> (string-length cert) 0)
                              (member? "SCRAM-SHA-256-PLUS" mechs string=?))
                         (scram/end-point cert)
                         (maybe:no)))
            ;; gs2 header: "p=..." we bind to the channel; "y" we could
            ;;  have, but the server didn't offer (so it can spot a
            ;;  downgrade); "n" we can't.
            (gs2 (match binding with
                   (maybe:yes _) -> "p=tls-server-end-point,,"
                   (maybe:no)    -> (if (> (string-length cert) 0) "y,," "n,,")))
            (mech (match binding with
                    (maybe:yes _) -> "SCRAM-SHA-256-PLUS"
                    (maybe:no)    -> "SCRAM-SHA-256"))
            (cbind (match binding with
                     (maybe:yes h) -> (string-append gs2 h)
                     (maybe:no)    -> gs2))
            (nonce (b64-encode ((urandom-make) 18)))
            ;; postgres takes the user name from the startup message.
            (first-bare (format "n=,r=" nonce)))
        (when (not (member? "SCRAM-SHA-256" mechs string=?))
          (raise (:PG/Unsupported (format "SASL mechanisms: " (join " " mechs)))))
        (sasl-initial mech (string-append gs2 first-bare))
        (let ((server-first (match (next) with
                              (:tuple #\R body)
                              -> (let ((c (pg/cursor body)))
                                   (if (= 11 (c.int32)) (c.rest) (raise (:PG/Protocol "expected SASLContinue"))))
                              (:tuple #\E body) -> (raise (:PG/Error (pg/parse-fields body)))
                              (:tuple t _) -> (unexpected t)))
              (snonce (scram/attr server-first #\r))
              (salt (b64-decode (scram/attr server-first #\s)))
              (iterations (string->int (scram/attr server-first #\i)))
              (_ (when (or (not (starts-with snonce nonce)) (= (string-length snonce) (string-length nonce)))
                   (raise (:PG/AuthFailed "server's SCRAM nonce doesn't extend ours"))))
              (_ (when (< iterations 1)
                   (raise (:PG/AuthFailed "bad SCRAM iteration count"))))
              (salted (pbkdf2/sha256 password salt iterations 32))
              (client-key (scram/hmac salted "Client Key"))
              (stored-key (sha256 client-key))
              (final-bare (format "c=" (b64-encode cbind) ",r=" snonce))
              (auth-message (format first-bare "," server-first "," final-bare))
              (proof (pbkdf2/xor client-key (scram/hmac stored-key auth-message)))
              (server-sig (scram/hmac (scram/hmac salted "Server Key") auth-message)))
          (send (pg/msg #\p (format final-bare ",p=" (b64-encode proof))))
          (match (next) with
            (:tuple #\R body)
            -> (let ((c (pg/cursor body)))
                 (when (not (= 12 (c.int32)))
                   (raise (:PG/Protocol "expected SASLFinal")))
                 (let ((server-final (c.rest)))
                   (when (starts-with server-final "e=")
                     (raise (:PG/AuthFailed (format "server: " (substring server-final 2 (string-length server-final))))))
                   ;; the server must prove it knows the password too.
                   (when (not (scram/same? (b64-decode (scram/attr server-final #\v)) server-sig))
                     (raise (:PG/AuthFailed "bad SCRAM server signature")))))
            (:tuple #\E body) -> (raise (:PG/Error (pg/parse-fields body)))
            (:tuple t _) -> (unexpected t))
          (match (next) with
            (:tuple #\R body)
            -> (when (not (= 0 (pg/first-int32 body)))
                 (raise (:PG/Protocol "expected AuthenticationOk after SASLFinal")))
            (:tuple #\E body) -> (raise (:PG/Error (pg/parse-fields body)))
            (:tuple t _) -> (unexpected t)))))

    (define (sasl-mechs c)
      (let loop ((acc (list:nil)))
        (let ((m (c.cstring)))
          (if (= 0 (string-length m))
              (reverse acc)
              (loop (list:cons m acc))))))

    (define (authenticate)
      (match (next) with
        (:tuple #\R body)
        -> (let ((c (pg/cursor body)))
             (match (c.int32) with
               0  -> #u ;; trust
               3  -> (begin
                       (send (pg/msg #\p (pg/cstring password)))
                       (authenticate))
               5  -> (raise (:PG/Unsupported "md5 password authentication (use scram-sha-256)"))
               10 -> (scram (sasl-mechs c))
               n  -> (raise (:PG/Unsupported (format "authentication method " (int n))))
               ))
        (:tuple #\E body) -> (raise (:PG/Error (pg/parse-fields body)))
        (:tuple t _) -> (unexpected t)))

    ;; after authentication: BackendKeyData, ParameterStatus (see
    ;;  next), then ReadyForQuery.
    (define (wait-ready)
      (match (next) with
        (:tuple #\K body)
        -> (let ((c (pg/cursor body)))
             (set! pid (c.int32))
             (set! secret (c.int32))
             (wait-ready))
        (:tuple #\Z body) -> (set! txn (int->char (pg/first-byte body)))
        (:tuple #\E body) -> (raise (:PG/Error (pg/parse-fields body)))
        (:tuple t _) -> (unexpected t)))

    ;; --- queries ---

    (define (parse-row-description body)
      (let ((c (pg/cursor body))
            (n (c.int16))
            (cols (list:nil)))
        (for-range i n
          (let ((name (c.cstring))
                (_ (c.bytes 6))   ;; table oid, attribute number
                (type (c.int32))
                (_ (c.bytes 8)))  ;; type size, modifier, format
            (push! cols {name=name type=type})))
        (reverse cols)))

    (define (parse-data-row body)
      (let ((c (pg/cursor body))
            (n (c.int16))
            (row (make-vector n (maybe:no))))
        (for-range i n
          (let ((len (c.int32)))
            (when (>= len 0)
              (set! row[i] (maybe:yes (c.bytes len))))))
        row))

    ;; read up to ReadyForQuery, collecting a result for each statement.
    ;;  an error is raised only then, so the connection is ready again.
    (define (collect)
      (let ((results (list:nil))
            (columns (list:nil))
            (rows (list:nil))
            (error (maybe:no))
            (copy? #f))
        (define (finish tag)
          (push! results {columns=columns rows=(reverse rows) tag=tag})
          (set! columns (list:nil))
          (set! rows (list:nil)))
        (let loop ()
          (match (next) with
            (:tuple #\T body) -> (begin (set! columns (parse-row-description body)) (set! rows (list:nil)) (loop))
            (:tuple #\D body) -> (begin (push! rows (parse-data-row body)) (loop))
            (:tuple #\C body) -> (begin (finish (pg/first-cstring body)) (loop))
            (:tuple #\I _)    -> (begin (finish "") (loop))
            (:tuple #\E body) -> (begin (set! error (maybe:yes (pg/parse-fields body))) (loop))
            ;; COPY: refuse COPY FROM STDIN; skip what COPY TO STDOUT sends.
            (:tuple #\G _)    -> (begin (set! copy? #t) (send (pg/msg #\f (pg/cstring "COPY is not supported by this client"))) (loop))
            (:tuple #\H _)    -> (begin (set! copy? #t) (loop))
            (:tuple #\W _)    -> (begin (set! copy? #t) (loop))
            (:tuple #\d _)    -> (loop)
            (:tuple #\c _)    -> (loop)
            ;; extended protocol: Parse, Bind, Close complete; NoData; PortalSuspended
            (:tuple #\1 _)    -> (loop)
            (:tuple #\2 _)    -> (loop)
            (:tuple #\3 _)    -> (loop)
            (:tuple #\n _)    -> (loop)
            (:tuple #\t _)    -> (loop)
            (:tuple #\s _)    -> (loop)
            (:tuple #\Z body) -> (set! txn (int->char (pg/first-byte body)))
            (:tuple t _)      -> (unexpected t)
            ))
        (match error with
          (maybe:yes fields) -> (raise (:PG/Error fields))
          (maybe:no) -> #u)
        (when copy?
          (raise (:PG/Unsupported "COPY")))
        (reverse results)))

    ;; a thread with a rows stream open holds the connection: if it
    ;;  started another query, it would wait for itself.
    (define (check-busy)
      (when (= stream-thread *thread-id*)
        (raise (:PG/Busy "a rows stream is still open on this connection: close it first (for-closing does)"))))

    (define (query sql)
      (check-busy)
      (pg/locked mutex
        (send (pg/msg #\Q (pg/cstring sql)))
        (collect)))

    (define (bind-execute stmt args)
      (string-append
       (pg/msg #\B (string-concat
                    (list (pg/cstring "")           ;; portal
                          (pg/cstring stmt)
                          (pg/int16 0)              ;; all parameters text
                          (pg/int16 (length args))
                          (string-concat
                           (map (lambda (arg)
                                  (match arg with
                                    (maybe:yes s) -> (string-append (pg/int32 (string-length s)) s)
                                    (maybe:no)    -> (pg/int32 -1)))
                                args))
                          (pg/int16 0))))           ;; all results text
       (pg/msg #\D (string-append "P" (pg/cstring "")))
       (pg/msg #\E (string-append (pg/cstring "") (pg/int32 0)))
       (pg/msg #\S "")))

    ;; just the Bind.
    (define (bind-execute-only stmt args)
      (pg/msg #\B (string-concat
                   (list (pg/cstring "")
                         (pg/cstring stmt)
                         (pg/int16 0)
                         (pg/int16 (length args))
                         (string-concat
                          (map (lambda (arg)
                                 (match arg with
                                   (maybe:yes s) -> (string-append (pg/int32 (string-length s)) s)
                                   (maybe:no)    -> (pg/int32 -1)))
                               args))
                         (pg/int16 0)))))

    (define (parse-msg stmt sql)
      (pg/msg #\P (string-concat (list (pg/cstring stmt) (pg/cstring sql) (pg/int16 0)))))

    (define (one results)
      (match results with
        (result) -> result
        _ -> (raise (:PG/Protocol (format "expected one result, got " (int (length results)))))))

    (define (execute sql args)
      (check-busy)
      (pg/locked mutex
        (send (string-append (parse-msg "" sql) (bind-execute "" args)))
        (one (collect))))

    (define (prepare sql)
      (let ((name (begin (inc! counter) (format "irken_" (int counter)))))
        (define (run args)
          (check-busy)
          (pg/locked mutex
            (send (bind-execute name args))
            (one (collect))))
        (define (close)
          (check-busy)
          (pg/locked mutex
            (send (string-append (pg/msg #\C (string-append "S" (pg/cstring name))) (pg/msg #\S "")))
            (collect)
            #u))
        (check-busy)
        (pg/locked mutex
          (send (string-append (parse-msg name sql) (pg/msg #\S "")))
          (collect))
        {run=run close=close}))

    ;; --- streaming ---

    ;; read and discard up to ReadyForQuery; the first error, if any.
    (define (drain)
      (let loop ((error (maybe:no)))
        (match (next) with
          (:tuple #\Z body) -> (begin (set! txn (int->char (pg/first-byte body))) error)
          (:tuple #\E body) -> (loop (match error with (maybe:no) -> (maybe:yes (pg/parse-fields body)) e -> e))
          _ -> (loop error))))

    ;; one statement, with its rows fetched <batch> at a time from a
    ;;  portal as they're wanted.  returns {columns rows}, where rows is
    ;;  a closable generator (see make-closable-generator) of rows.
    ;;
    ;; the connection is held (other threads wait, and this thread's
    ;;  other queries raise PG/Busy) until the stream ends or is closed:
    ;;  use for-closing, or close it yourself.  an error part way raises
    ;;  PG/Error from the generator, after the rows before it.
    (define (rows* sql args batch)
      (check-busy)
      (mutex.acquire)
      (set! stream-thread *thread-id*)
      (let ((open? #t)       ;; is the portal open (no Sync sent)?
            (held? #t))      ;; do we hold the mutex?

        (define (execute-more)
          (send (string-append (pg/msg #\E (string-append (pg/cstring "") (pg/int32 batch)))
                               (pg/msg #\H ""))))     ;; Flush: keep the portal

        ;; Sync (closing the portal first if it's open), and read up to
        ;;  ReadyForQuery.  returns the first error, if any.
        (define (finish)
          (let ((error (if open?
                           (begin
                             (set! open? #f)
                             (send (string-append (pg/msg #\C (string-append "P" (pg/cstring "")))
                                                  (pg/msg #\S "")))
                             (drain))
                           (maybe:no))))
            error))

        (define (release)
          (when held?
            (set! held? #f)
            (set! stream-thread -1)
            (mutex.release)))

        ;; for close, and for the producer's finally: safe to call twice.
        (define (cleanup)
          (try
           (finish)
           finally
           (release))
          #u)

        (define (fail fields)
          (cleanup)
          (raise (:PG/Error fields)))

        ;; Parse, Bind, Describe, and the first batch.
        (send (string-concat
               (list (parse-msg "" sql)
                     (bind-execute-only "" args)
                     (pg/msg #\D (string-append "P" (pg/cstring "")))
                     (pg/msg #\E (string-append (pg/cstring "") (pg/int32 batch)))
                     (pg/msg #\H ""))))

        ;; up to the RowDescription (or NoData).
        (let ((columns
               (try
                (let loop ()
                  (match (next) with
                    (:tuple #\1 _) -> (loop)
                    (:tuple #\2 _) -> (loop)
                    (:tuple #\T body) -> (parse-row-description body)
                    (:tuple #\n _) -> (list:nil)
                    (:tuple #\E body) -> (begin
                                            ;; the server skips to Sync.
                                            (set! open? #f)
                                            (send (pg/msg #\S ""))
                                            (drain)
                                            (release)
                                            (raise (:PG/Error (pg/parse-fields body))))
                    (:tuple t _) -> (unexpected t)))
                except
                (:PG/Error f) -> (raise (:PG/Error f))
                (:Reader/Closed why) -> (begin (release) (raise (:Reader/Closed why)))
                (:PG/Protocol why) -> (begin (release) (raise (:PG/Protocol why)))))
              (gen
               (closable-gen emit
                 (try
                  (let loop ()
                    (match (next) with
                      (:tuple #\D body) -> (begin (emit (parse-data-row body)) (loop))
                      (:tuple #\s _)    -> (begin (execute-more) (loop))    ;; PortalSuspended
                      (:tuple #\C _)    -> #u                                ;; all done
                      (:tuple #\I _)    -> #u
                      (:tuple #\E body) -> (begin
                                              ;; the portal is gone; the server skips to Sync.
                                              (set! open? #f)
                                              (send (pg/msg #\S ""))
                                              (match (drain) with _ -> #u)
                                              (release)
                                              (raise (:PG/Error (pg/parse-fields body))))
                      (:tuple t _)      -> (unexpected t)))
                  finally
                  (cleanup)))))
          {columns = columns
           rows    = {next  = gen.next
                      ;; (if the consumer never asked for a row, the
                      ;;  producer never started, and closing it does
                      ;;  nothing: so clean up here too.)
                      close = (lambda () (gen.close) (cleanup))}})))

    (define (rows sql args)
      (rows* sql args 500))

    (define (parameter name)
      (let loop ((l params))
        (match l with
          () -> (maybe:no)
          ((:tuple n v) . tl) -> (if (string=? n name) (maybe:yes v) (loop tl))
          )))

    (define (notices)
      (let ((r (reverse notes)))
        (set! notes (list:nil))
        r))

    (define (close)
      (try
       (send (pg/msg #\X ""))
       except
       (:OSError _) -> #u)
      (conn.close))

    ;; --- body of pg/client ---
    (startup)
    (authenticate)
    (wait-ready)

    {query     = query
     execute   = execute
     prepare   = prepare
     rows      = rows
     rows-batch = rows*
     parameter = parameter
     status    = (lambda () txn)
     pid       = (lambda () pid)
     notices   = notices
     close     = close
     cancel    = (lambda () (cancel pid secret))
     }
    ))

;; connect to <host>:<port> (at <ip>, if not ""), with <tls>, and log in.
;;  <verify> checks the server's certificates (see net/check-cert); it
;;  isn't used with (pg-tls:disable).
(define (pg/connect host port ip tls verify user password database)
  (let ((ip (if (= 0 (string-length ip)) (net/resolve-host host) ip))
        (sock (doom/make (tcp4-sock))))

    ;; a CancelRequest goes on a connection of its own (in the clear:
    ;;  it holds only the process id and the secret).
    (define (cancel pid secret)
      (let ((s (doom/make (tcp4-sock))))
        (s.connect (address/make4 ip port))
        (s.send (string-append (pg/int32 16) (pg/int32 80877102) (pg/int32 pid) (pg/int32 secret)))
        (s.close)))

    (sock.connect (address/make4 ip port))
    (match tls with
      (pg-tls:disable)
      -> (pg/client (doom/stream sock) user password database "" cancel)
      (pg-tls:require)
      -> (let ((plain (reader/make sock)))
           (sock.send (string-append (pg/int32 8) (pg/int32 80877103))) ;; SSLRequest
           (let ((answer (plain.read-n 1)))
             (when (not (string=? answer "S"))
               (sock.close)
               (raise (:PG/NoTLS)))
             ;; anything after the 'S' came before the handshake,
             ;;  unprotected (cf. CVE-2021-23222).
             (when (> (plain.ready) 0)
               (sock.close)
               (raise (:PG/Protocol "data received after the SSLRequest answer")))
             (let ((t (tls13/client sock host (list "postgresql") verify))
                   (info (t.get-info)))
               (pg/client (doom/stream t) user password database (first info.certs) cancel))))
      )))
