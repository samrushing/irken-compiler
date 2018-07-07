;; -*- Mode: Irken -*-

;; -----------------
;; TLS-1.3, RFC XXXX
;; -----------------
;;
;; supported diffie-hellman groups:
;; X25519, X448
;;
;; supported signature algorithms (i.e., cert key types):
;; RSA (currently unsafe & slow), Ed25519, Ed448
;;
;; supported cipher suites:
;; chacha20-poly1305-sha256, aes-256-gcm-sha384
;;
;; for now, using only libsodium and our own bignum RSA (needs blinding!).
;; until ed448 is added to libsodium, use libdecaf.
;; libraries to consider for ECDSA: micro-ecc/easy-ecc?, crypto++.
;;
;; TODO:
;;  * multiple/fragmented records
;;  * HelloRetryRequest
;;
;; FUTURE:
;;  * client side (will need lots of work, OCSP etc)

(define RNG (urandom-make))

;; [the remaining three are aes-128 gcm & ccm]
(define suite-priority-list
  (LIST (cipher-suite:chacha20-poly1305-sha256)
        (cipher-suite:aes-256-gcm-sha384)))

(define kex-priority-list
  (LIST (named-group:x25519)
        (named-group:x448)))

;; XXX maybe need a record to hold all these params (and include socket buffer sizes)

(define (tls13/make-channel sockfun certs skey alpns)

  (let ((sock (sockfun 8192 8192))
        (ibuf (buffer/make 8192))
        (obuf (buffer/make 8192))
        (state (tls-state:start))
        (draft-version 0)
        (kex (make-kex (named-group:x25519)))
        (client-share "")
        (client-verify "")
        (info (list:nil))
        (client-hello-sexp (sexp:undef))
        (selected-suite (cipher-suite:chacha20-poly1305-sha256))
        (signer (make-signer skey))
        (extensions-to-push (list:nil))
        (ksched (keysched/make sha256-hash aead-chacha20poly1305 #x304)))

    (define (set-state! new)
      (printf "[" (sym (tls-state->name new)) "]")
      (set! state new))

    (define (choose-version versions)
      (if (member-eq? #x0304 versions) ;; final version
          #x0304
          ;; pick the highest draft version reported.
          (let ((drafts (sort > (filter (lambda (x) (= #x7f (>> x 8))) versions))))
            (if (null? drafts)
                -1
                (first drafts)))))

    (define (supports-tls13? ch)
      (match (find-extension 'supported-versions ch.exts) with
        (maybe:yes (tlsext:supported-versions versions))
        -> (choose-version versions)
        _ -> -1
        ))

    (define (find-share groups shares)
      (define search
        group        () -> (maybe:no)
        group (hd . tl) -> (if (= group hd.group) (maybe:yes hd.kex) (search group tl)))
      (match groups with
        ()               -> (maybe:no)
        (group . groups) -> (match (search (named-group->int group) shares) with
                              (maybe:yes kex) -> (maybe:yes {group=group kex=kex})
                              (maybe:no)      -> (find-share groups shares))
        ))

    (define (find-keyshare ch)
      (match (find-extension 'client-shares ch.exts) with
        (maybe:yes (tlsext:client-shares shares))
        -> (match (find-share kex-priority-list shares) with
             (maybe:yes pair) -> (begin (set! client-share pair.kex) (set! kex (make-kex pair.group)) (kex.gen))
             (maybe:no)       -> (raise (:TLS/Alert (tls-alert-desc:hsk-failure) "no shared group")))
        _ -> (raise (:TLS/Alert (tls-alert-desc:missing-extension) "missing client-shares extension"))
        ))

    (define (find-sigalg ch)
      (match (find-extension 'sigalgs ch.exts) with
        (maybe:yes (tlsext:sigalgs algos))
        -> (if (member-eq? (sigalg->int signer.sigalg) algos)
               #u
               (raise (:TLS/Alert (tls-alert-desc:hsk-failure) "no compatible signature algorithm")))
        _ -> (raise (:TLS/Alert (tls-alert-desc:missing-extension) "no signature-algorithms extension"))
        ))

    (define (find-alpn ch)
      (match (find-extension 'alpn ch.exts) with
        (maybe:yes (tlsext:alpn client-alpns))
        -> (let loop ((salpns alpns))
             (match salpns with
               () -> (raise (:TLS/Alert (tls-alert-desc:no-application-protocol) "no matching ALPN"))
               (alpn . rest)
               -> (if (member? alpn client-alpns string=?)
                      (PUSH extensions-to-push (tlsext:alpn (LIST alpn)))
                      (loop rest))))
        ;; XXX should this be a policy decision?
        _ -> (raise (:TLS/Alert (tls-alert-desc:no-application-protocol) "no ALPN extension present"))
        ))

    (define (handle-client-hello pkt ch)
      (let ((version (supports-tls13? ch)))
        (when (= -1 version)
          (raise (:TLS/Alert (tls-alert-desc:protocol-version) "no tls-1.3 support")))
        (when (> version 0)
          (printf "protocol version: " (zpad 4 (hex version)) "\n"))
        (set! draft-version version)
        (PUSH info (sexp (sym 'version) (int version)))
        (PUSH info (client-hello->sexp ch))
        ;;(pp (client-hello->sexp ch) 132)
        (find-sigalg ch)
        (find-keyshare ch)
        (when (not (null? alpns))
          (find-alpn ch))
        (set-state! (tls-state:rcvch))
        (send-server-hello pkt ch version)
        ))

    (define suite->keysched
      (cipher-suite:chacha20-poly1305-sha256) -> (keysched/make sha256-hash aead-chacha20poly1305 draft-version)
      (cipher-suite:aes-256-gcm-sha384)       -> (keysched/make sha384-hash aead-aes256gcm draft-version)
      _                                       -> (raise (:TLS/Alert (tls-alert-desc:hsk-failure) "no shared suite"))
      )

    (define select-suite
      () suites
      -> (raise (:TLS/Alert (tls-alert-desc:hsk-failure) "no shared suite"))
      (hd . tl) suites
      -> (let ((code (cipher-suite->int hd)))
           (if (member-eq? code suites)
               (begin
                 (set! selected-suite hd)
                 (set! ksched (suite->keysched hd))
                 code)
               (select-suite tl suites))))

    (define (send-server-hello ch-pkt ch version)
      (let ((suite (select-suite suite-priority-list ch.suites))
            (sh {random = (RNG 32)
                 sessid = ch.sessid
                 suite = suite
                 exts = (LIST (tlsext:supported-versions (LIST version)) ;; only one
                              (tlsext:key-share {group=(named-group->int kex.group) kex=(kex.get-pub)}))})
            (shared-key (kex.gen-shared client-share))
            (server-hello (pack-hsk (tls-hsk-type:server-hello) (pack-server-hello sh)))
            (enc-exts (pack-hsk (tls-hsk-type:enc-exts) (pack-enc-exts extensions-to-push)))
            (cert (pack-hsk (tls-hsk-type:cert) (put-cert certs))))
        (ksched.add-tscript-packet ch-pkt)
        (send-packet (tls-ctype:hsk) server-hello)
        (set-state! (tls-state:negot))
        (ksched.add-tscript-packet (rope->string server-hello))
        (ksched.set-hsk-key shared-key)
        (ksched.add-tscript-packet (rope->string enc-exts))
        (PUSH info (sexp1 'encrypted-extensions (map extension->sexp extensions-to-push)))
        (ksched.add-tscript-packet (rope->string cert))
        (let ((tscript-hash (ksched.get-tscript-hash))
              (tbs (format (repeat 64 " ") "TLS 1.3, server CertificateVerify\x00" tscript-hash))
              (sig (signer.sign tbs RNG))
              (sigint (sigalg->int signer.sigalg))
              (cert-verify (pack-hsk (tls-hsk-type:cert-verify) (put-cert-verify sigint sig)))
              (_ (ksched.add-tscript-packet (rope->string cert-verify)))
              (finished (pack-hsk (tls-hsk-type:finished) (rope:leaf (ksched.get-finished #t)))))
          (send-encrypted-packet (tls-ctype:hsk) (rope/build enc-exts cert cert-verify finished) 0)
          (ksched.add-tscript-packet (rope->string finished))
          (set! client-verify (ksched.get-finished #f))
          (set-state! (tls-state:wfini))
          )))

    (define (handle-appdata protected)
      (let (((kind plaintext) (decrypt-packet protected))
            (ptlen (string-length plaintext)))
        (match (unpack-record kind plaintext 0) with
          (tls-record:appdata user-data)
          -> (buffer/add! ibuf user-data)
          other
          -> (handle-record {raw=plaintext record=other kind=kind version=#x303 length=ptlen})
          )))

    ;; XXX handle fragmented packets.  See section C.3 for "implementation pitfalls".
    (define (send-packet kind pkt)
      (sock.send
       (rope->string
        (rope/build
         (pack-header kind (rope-length pkt))
         pkt))))

    (define (send-encrypted-packet kind pkt npad)
      (sock.send (ksched.encrypt-packet kind pkt npad)))

    (define (send-alert desc)
      (let ((pkt (pack-alert (tls-alert-level:fatal) desc)))
        (match state with
          (tls-state:cnctd) -> (send-encrypted-packet (tls-ctype:alert) pkt 0)
          otherwise         -> (send-packet (tls-ctype:alert) pkt)
          )))

    ;; XXX handle multiple records!
    (define (find-true-type pt)
      ;; seek past padding to find the true tls-ctype
      (let loop ((i (- (string-length pt) 1)))
        (match i (char->int (string-ref pt i))
          with
          0 _ -> (raise (:TLS/Alert (tls-alert-desc:decode-error) "no type in decrypted packet"))
          _ 0 -> (loop (- i 1))
          _ k -> (:tuple k (substring pt 0 i))
          )))

    (define (decrypt-packet protected)
      (let ((header (rope->string (pack-header (tls-ctype:appdata) (string-length protected)))))
        (find-true-type (ksched.decrypt-packet protected header))))

    (define (handle-change-cipher-spec pkt)
      (when (not (= 1 (string-length pkt.raw)))
        (raise (:TLS/Alert (tls-alert-desc:unexpected-message) "non-empty ChangeCipherSpec"))))

    ;; Note: tls-1.3 deprecates the alert 'level' distinction, and instead
    ;;  divides alerts into two classes:
    ;;  closure alerts: close_notify, user_canceled
    ;;  error alerts: all others

    (define (handle-alert-close)
      (set-state! (tls-state:closed)))

    (define handle-alert
      (tls-alert-desc:close-notify)  -> (handle-alert-close)
      (tls-alert-desc:user-canceled) -> (handle-alert-close)
      other
      -> (begin (set-state! (tls-state:closed))
                (raise (:TLS/ReceivedFatalAlert other))))

    (define (handle-finished verify-data)
      (when (not (constant-time-string=? verify-data client-verify))
        (raise (:TLS/Alert (tls-alert-desc:decrypt-error) "client verify data")))
      (ksched.set-appdata-key)
      (set-state! (tls-state:cnctd))
      )

    (define (handle-record pkt)
      ;;(printf "**** state == " (sym (tls-state->name state)) "\n")
      (match state pkt.record with
        (tls-state:start) (tls-record:hsk (tls-hsk:client-hello ch)) -> (handle-client-hello pkt.raw ch)
        (tls-state:wfini) (tls-record:hsk (tls-hsk:finished vdat))   -> (handle-finished vdat)
        (tls-state:wfini) (tls-record:change-cipher-spec)            -> (handle-change-cipher-spec pkt)
        (tls-state:wfini) (tls-record:appdata protected)             -> (handle-appdata protected)
        (tls-state:cnctd) (tls-record:appdata protected)             -> (handle-appdata protected)
        any               (tls-record:alert level desc)              -> (handle-alert desc)
        any rec ;; XXX attach a hex version of the offending record here.
        -> (raise (:TLS/Alert (tls-alert-desc:unexpected-message) "unexpected record"))
        ))

    ;; XXX handle fragmented packets.  See section C.3 for "implementation pitfalls".
    ;; XXX handle multiple records!
    (define (read-record)
      (let (((kind version length pos) (unpack-header (sock.recv-exact 5) 0))
            (pkt (sock.recv-exact length))
            (record (unpack-record kind pkt 0)))
        {raw=pkt record=record kind=kind version=version length=length}
        ))

    ;; --- socket layer ---

    (define (sock/recv)
      (if (eq? state (tls-state:cnctd))
          (cond ((> (buffer/ready ibuf) 0)
                 (buffer/get-all! ibuf))
                (else
                 (handle-record (read-record))
                 (sock/recv)))
          (raise (:Doom/EOF sock.sock))))

    (define (sock/recv-exact n)
      (if (eq? state (tls-state:cnctd))
          (cond ((>= (buffer/ready ibuf) n)
                 (buffer/get! ibuf n))
                (else
                 (handle-record (read-record))
                 (sock/recv-exact n)))
          (raise (:Doom/EOF sock.sock))))

    ;; XXX need to use obuf here (i.e. split into record-sized chunks)
    (define (sock/send-rope rope-data npad)
      (if (eq? state (tls-state:cnctd))
          (send-encrypted-packet (tls-ctype:appdata) rope-data npad)
          (raise (:Doom/EOF sock.sock))))

    (define (sock/send data)
      (sock/send-rope (rope:leaf data) 0))

    (define (sock/close)
      (when (eq? state (tls-state:cnctd))
        (send-alert (tls-alert-desc:close-notify))
        (set-state! (tls-state:closed))))

    (define (sock/get-fd)
      (sock.get-fd))

    (define (sock/get-info)
      (sexp
       (sym 'tls-info)
       (sexp (sym 'selected)
             (sexp (sym 'kex) (sym (named-group->name kex.group)))
             (sexp (sym 'suite) (sym (cipher-suite->name selected-suite)))
             (sexp (sym 'signature-algorithm) (sym (sigalg->name signer.sigalg))))
       (sexp1 'protocol (reverse info))
       ))

    ;; --- create socket object ---
    ;; XXX TODO?: half-closed connections.
    ;; get connected
    (let loop ()
      (try
       (begin
         (handle-record (read-record))
         (if (not (eq? state (tls-state:cnctd)))
             (loop)
             #u))
       except
       (:TLS/Alert desc msg)
       -> (begin (printf "sending tls alert: '" (sym (tls-alert-desc->name desc)) "' " msg "\n")
                 (set-state! (tls-state:closed))
                 (send-alert desc)
                 #u)
       (:TLS/ReceivedFatalAlert desc)
       -> (begin (printf "received tls alert: '" (sym (tls-alert-desc->name desc)) "\n")
                 (set-state! (tls-state:closed)) ;; half-closed?
                 #u)
       (:Doom/EOF s)
       -> (begin (printf "EOF\n")
                 #u)
       ))
    (when (not (eq? state (tls-state:cnctd)))
      (raise (:TLS/Failed)))
    ;; return our doom-tls-socket object
    {recv       = sock/recv
     recv-exact = sock/recv-exact
     send       = sock/send
     send-rope  = sock/send-rope
     close      = sock/close
     get-fd     = sock/get-fd
     get-info   = sock/get-info
     }
    ))
