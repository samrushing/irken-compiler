;; -*- Mode: Irken -*-

;; ------------------------------
;; TLS-1.3 client, RFC 8446
;; ------------------------------
;;
;; key exchange: X25519
;; cipher suites: chacha20-poly1305-sha256 (and aes-256-gcm-sha384, see below)
;; server signatures: ecdsa-secp256r1-sha256, ecdsa-secp384r1-sha384,
;;   ed25519, rsa-pss-rsae-sha{256,384,512}
;;
;; not supported: HelloRetryRequest, client certificates, resumption
;;  (NewSessionTicket is ignored), 0-RTT, KeyUpdate.
;;
;; checking the server's certificates is up to the caller: see
;;  tls13/connect and lib/crypto/x509path.scm.

(require "lib/urandom.scm")
(require "lib/net/socket.scm")
(require "lib/crypto/x509path.scm")

(require "doom/tls/kex.scm")
(require "doom/tls/keysched.scm")

;; XXX aes-256-gcm-sha384 works (see suite->keysched below), but
;;  libsodium only does AES-GCM on CPUs with AES instructions, and we
;;  can't yet ask it (crypto_aead_aes256gcm_is_available, after
;;  sodium_init).  so for now, offer only chacha20.
(define tls13-client-suites
  (list (cipher-suite:chacha20-poly1305-sha256)))

;; what we can check in CertificateVerify (the first six), plus what
;;  we can check in certificates.
(define tls13-client-sigalgs
  (list (sigalg:ecdsa-secp256r1-sha256)
        (sigalg:ecdsa-secp384r1-sha384)
        (sigalg:ed25519)
        (sigalg:rsa-pss-rsae-sha256)
        (sigalg:rsa-pss-rsae-sha384)
        (sigalg:rsa-pss-rsae-sha512)
        (sigalg:ecdsa-secp521r1-sha512)
        (sigalg:rsa-pkcs1-sha256)
        (sigalg:rsa-pkcs1-sha384)
        (sigalg:rsa-pkcs1-sha512)))

(datatype tlsc-state
  (:wait-sh)    ;; sent ClientHello
  (:wait-ee)    ;; got ServerHello
  (:wait-cert)  ;; got EncryptedExtensions
  (:wait-cv)    ;; got Certificate
  (:wait-fin)   ;; got CertificateVerify
  (:cnctd)
  (:closed)
  )

(define (put-client-ext code body)
  (rope/build (put-u16 code) (put-var-16 body)))

(define (pack-client-hello random server-name alpns key-share)
  (let ((exts (list:nil)))
    ;; no SNI for an IP address (rfc6066 3)
    (when (not (ipv4-literal? server-name))
      (push! exts (put-client-ext 0 (put-var-16 (rope/build (put-u8 0) (put-opaque-16 server-name))))))
    (push! exts (put-client-ext 10 (put-vector-16 put-u16 (list (named-group->int key-share.group)))))
    (push! exts (put-client-ext 13 (put-vector-16 put-u16 (map sigalg->int tls13-client-sigalgs))))
    (when (not (null? alpns))
      (push! exts (put-client-ext 16 (put-vector-16 put-opaque-8 alpns))))
    (push! exts (put-client-ext 43 (put-vector-8 put-u16 (list #x304))))
    (push! exts (put-client-ext 51 (put-vector-16 put-key-share
                                                  (list {group=(named-group->int key-share.group)
                                                         kex=key-share.kex}))))
    (rope/build
     (put-u16 #x303)            ;; legacy_version
     (put-opaque random)        ;; 32 bytes
     (put-opaque-8 "")          ;; legacy_session_id
     (put-vector-16 put-u16 (map cipher-suite->int tls13-client-suites))
     (put-opaque-8 "\x00")      ;; legacy_compression_methods: null
     (put-var-16 (rope/cat (reverse exts))))))

;; check a CertificateVerify signature <sig> over <content> by the
;;  server's public key <key>.
(define (tls13-verify-signature scheme key content sig)
  (define (ec curve-oid c hash k)
    (and (oid=? curve-oid c.oid)
         (ecdsa-verify c (hash content) sig k)))
  (match (alist/lookup sigalg-rev-alist scheme) key with
    (maybe:yes (sigalg:ecdsa-secp256r1-sha256)) (public-key:ec k curve) -> (ec curve ec-curve-p256 sha256 k)
    (maybe:yes (sigalg:ecdsa-secp384r1-sha384)) (public-key:ec k curve) -> (ec curve ec-curve-p384 sha384 k)
    (maybe:yes (sigalg:ed25519)) (public-key:ed25519 pk)  -> (ed25519-verify sig content pk)
    (maybe:yes (sigalg:rsa-pss-rsae-sha256)) (public-key:rsa n e) -> (rsassa-pss-verify n (int->big e) content sig sha256-hash)
    (maybe:yes (sigalg:rsa-pss-rsae-sha384)) (public-key:rsa n e) -> (rsassa-pss-verify n (int->big e) content sig sha384-hash)
    (maybe:yes (sigalg:rsa-pss-rsae-sha512)) (public-key:rsa n e) -> (rsassa-pss-verify n (int->big e) content sig sha512-hash)
    _ _ -> (raise (:TLS/Alert (tls-alert-desc:illegal-parameter)
                              (format "server used signature scheme " (sigalg->string scheme)
                                      " with a " (public-key-kind key) " key")))
    ))

;; perform a TLS-1.3 handshake over <sock> (a connected doom socket).
;;  <server-name> is sent in SNI; <alpns> are the application
;;  protocols we offer (e.g. '("h2")).  <verify> is called with the
;;  server's certificates (DER, leaf first) and must return the parsed
;;  leaf certificate, or raise.
;;
;; returns a socket-like object (like tls13/make-channel's), or raises
;;  (:TLS/ClientFailed reason).
(define (tls13/client sock server-name alpns verify)

  (let ((RNG (urandom-make))
        (ibuf (buffer/make 8192))
        (maxfraglen (<< 1 14))
        (state (tlsc-state:wait-sh))
        (kex (make-kex (named-group:x25519)))
        (client-hello "")
        (ksched (keysched/make sha256-hash aead-chacha20poly1305 #x304))
        (suite (cipher-suite:chacha20-poly1305-sha256))
        (alpn (maybe:no))
        (certs (list:nil))
        (leaf (maybe:no))
        (sigalg 0)
        (failure "")
        (clean-close #f)) ;; did the server send close_notify?

    ;; --- sending ---

    (define (send-fragment kind frag)
      (sock.send (rope->string (rope/build (pack-header kind (string-length frag)) (rope:leaf frag)))))

    (define (send-encrypted-fragment kind frag)
      (sock.send (ksched.encrypt-packet kind (rope:leaf frag) 0)))

    ;; split into records of at most maxfraglen.
    (define (send-split send kind pkt)
      (let ((spkt (rope->string pkt))
            (plen (string-length spkt)))
        (let loop ((pos 0))
          (when (< pos plen)
            (let ((end (min plen (+ pos maxfraglen))))
              (send kind (substring spkt pos end))
              (loop end))))))

    (define (send-packet kind pkt)
      (send-split send-fragment kind pkt))

    (define (send-encrypted-packet kind pkt)
      (send-split send-encrypted-fragment kind pkt))

    (define (encrypted-state?)
      (match state with
        (tlsc-state:wait-sh) -> #f
        (tlsc-state:closed)  -> #f
        _                    -> #t
        ))

    (define (send-alert desc)
      (let ((pkt (pack-alert (tls-alert-level:fatal) desc)))
        (try
         (if (encrypted-state?)
             (send-encrypted-packet (tls-ctype:alert) pkt)
             (send-packet (tls-ctype:alert) pkt))
         except
         (:OSError _) -> #u
         (:Sodium/APIFail _) -> #u)))

    ;; --- receiving ---

    (define (find-true-type pt)
      ;; seek past padding to find the true tls-ctype
      (let loop ((i (- (string-length pt) 1)))
        (cond ((< i 0) (raise (:TLS/Alert (tls-alert-desc:unexpected-message) "no type in decrypted record")))
              ((= 0 (char->int (string-ref pt i))) (loop (- i 1)))
              (else (:tuple (char->int (string-ref pt i)) (substring pt 0 i))))))

    (define (read-fragment)
      (let (((kind version length) (unpack-header (sock.recv-exact 5)))
            (frag (sock.recv-exact length)))
        (if (and (encrypted-state?) (= kind (tls-ctype->int (tls-ctype:appdata))))
            (let ((header (rope->string (pack-header (tls-ctype:appdata) length))))
              (find-true-type
               (try
                (ksched.decrypt-packet frag header)
                except
                (:Sodium/APIFail _)
                -> (raise (:TLS/Alert (tls-alert-desc:bad-record-mac) "can't decrypt record")))))
            (:tuple kind frag))))

    ;; --- the handshake ---

    (define (send-client-hello)
      (kex.gen)
      (let ((ch (pack-hsk (tls-hsk-type:client-hello)
                          (pack-client-hello (RNG 32) server-name alpns
                                             {group=kex.group kex=(kex.get-pub)}))))
        (set! client-hello (rope->string ch))
        (send-packet (tls-ctype:hsk) ch)))

    (define (suite->keysched code)
      (match (alist/lookup cipher-suite-rev-alist code) with
        (maybe:yes (cipher-suite:chacha20-poly1305-sha256))
        -> (begin (set! suite (cipher-suite:chacha20-poly1305-sha256))
                  (keysched/make sha256-hash aead-chacha20poly1305 #x304))
        (maybe:yes (cipher-suite:aes-256-gcm-sha384))
        -> (begin (set! suite (cipher-suite:aes-256-gcm-sha384))
                  (keysched/make sha384-hash aead-aes256gcm #x304))
        _ -> (raise (:TLS/Alert (tls-alert-desc:illegal-parameter)
                                (format "server chose cipher suite " (suite->string code))))
        ))

    (define (handle-server-hello raw sh)
      (match (find-extension 'hrr-key-share sh.exts) with
        (maybe:yes (tlsext:hrr-key-share group))
        -> (raise (:TLS/Alert (tls-alert-desc:hsk-failure)
                              (format "server wants key exchange group " (named-group->string group)
                                      " (HelloRetryRequest is not supported)")))
        _ -> #u)
      (match (find-extension 'supported-versions sh.exts) with
        (maybe:yes (tlsext:supported-versions (#x304))) -> #u
        _ -> (raise (:TLS/Alert (tls-alert-desc:protocol-version) "server does not support TLS 1.3")))
      (when (not (string=? sh.sessid ""))
        (raise (:TLS/Alert (tls-alert-desc:illegal-parameter) "legacy_session_id not echoed")))
      (match (find-extension 'key-share sh.exts) with
        (maybe:yes (tlsext:key-share share))
        -> (begin
             (when (not (= share.group (named-group->int kex.group)))
               (raise (:TLS/Alert (tls-alert-desc:illegal-parameter) "server chose another group")))
             (set! ksched (suite->keysched sh.suite))
             (ksched.set-client)
             (ksched.add-tscript-packet client-hello)
             (ksched.add-tscript-packet raw)
             (ksched.set-hsk-key (kex.gen-shared share.kex))
             (set! state (tlsc-state:wait-ee)))
        _ -> (raise (:TLS/Alert (tls-alert-desc:missing-extension) "no key_share from server"))
        ))

    (define (handle-enc-exts raw exts)
      (ksched.add-tscript-packet raw)
      (match (find-extension 'alpn exts) with
        (maybe:yes (tlsext:alpn (proto)))
        -> (if (member? proto alpns string=?)
               (set! alpn (maybe:yes proto))
               (raise (:TLS/Alert (tls-alert-desc:illegal-parameter) "server chose an ALPN we didn't offer")))
        _ -> #u)
      (set! state (tlsc-state:wait-cert)))

    (define (handle-cert raw ders)
      (ksched.add-tscript-packet raw)
      (set! certs ders)
      (set! leaf (maybe:yes (verify ders)))
      (set! state (tlsc-state:wait-cv)))

    (define (handle-cert-verify raw scheme sig)
      (let ((content (format (repeat 64 " ") "TLS 1.3, server CertificateVerify\x00"
                             (ksched.get-tscript-hash))))
        (match leaf with
          (maybe:yes x)
          -> (when (not (tls13-verify-signature scheme x.cert.pubkey.key content sig))
               (raise (:TLS/Alert (tls-alert-desc:decrypt-error) "bad CertificateVerify signature")))
          (maybe:no) -> (impossible))
        (set! sigalg scheme)
        (ksched.add-tscript-packet raw)
        (set! state (tlsc-state:wait-fin))))

    (define (handle-finished raw verify-data)
      (when (not (constant-time-string=? verify-data (ksched.get-finished #t)))
        (raise (:TLS/Alert (tls-alert-desc:decrypt-error) "bad server Finished")))
      (ksched.add-tscript-packet raw)
      (let ((fin (pack-hsk (tls-hsk-type:finished) (rope:leaf (ksched.get-finished #f)))))
        ;; the application keys come from the transcript up to the
        ;;  server's Finished, so set them after sending ours.
        (send-encrypted-packet (tls-ctype:hsk) fin)
        (ksched.set-appdata-key)
        (set! state (tlsc-state:cnctd))))

    (define (handle-alert desc)
      (set! state (tlsc-state:closed))
      (match desc with
        (tls-alert-desc:close-notify)  -> (set! clean-close #t)
        (tls-alert-desc:user-canceled) -> #u
        _ -> (raise (:TLS/ReceivedFatalAlert desc))
        ))

    (define (handle-record pkt)
      (match state pkt.record with
        (tlsc-state:wait-sh)   (tls-record:hsk (tls-hsk:server-hello sh))       -> (handle-server-hello pkt.raw sh)
        (tlsc-state:wait-ee)   (tls-record:hsk (tls-hsk:enc-exts exts))         -> (handle-enc-exts pkt.raw exts)
        (tlsc-state:wait-cert) (tls-record:hsk (tls-hsk:cert ders))             -> (handle-cert pkt.raw ders)
        (tlsc-state:wait-cert) (tls-record:hsk (tls-hsk:cert-request))
        -> (raise (:TLS/Alert (tls-alert-desc:cert-required) "server wants a client certificate"))
        (tlsc-state:wait-cv)   (tls-record:hsk (tls-hsk:cert-verify scheme sig)) -> (handle-cert-verify pkt.raw scheme sig)
        (tlsc-state:wait-fin)  (tls-record:hsk (tls-hsk:finished vd))           -> (handle-finished pkt.raw vd)
        (tlsc-state:cnctd)     (tls-record:appdata plaintext)                   -> (buffer/add! ibuf plaintext)
        (tlsc-state:cnctd)     (tls-record:hsk (tls-hsk:new-session-ticket))    -> #u
        ;; middlebox compatibility mode (rfc8446 D.4)
        _                      (tls-record:change-cipher-spec)                  -> #u
        _                      (tls-record:alert level desc)                    -> (handle-alert desc)
        _ _ -> (raise (:TLS/Alert (tls-alert-desc:unexpected-message) "unexpected message"))
        ))

    (define (read-records)
      (for-list record (get-records read-fragment)
        (handle-record record)))

    ;; --- socket layer ---

    (define (check-open)
      (when (not (eq? state (tlsc-state:cnctd)))
        (raise (:Doom/EOF sock.sock))))

    ;; read more application data.  raises Doom/EOF once the
    ;;  connection is closed: either cleanly with close_notify, or
    ;;  (see sock/get-info's clean-close) by TCP alone.
    (define (fill)
      (check-open)
      (try
       (read-records)
       except
       (:Doom/EOF s)
       -> (begin
            (set! state (tlsc-state:closed))
            (raise (:Doom/EOF s)))))

    ;; data already received is returned before any EOF.
    (define (sock/recv)
      (if (> (buffer/ready ibuf) 0)
          (buffer/get-all! ibuf)
          (begin (fill) (sock/recv))))

    (define (sock/recv-exact n)
      (if (>= (buffer/ready ibuf) n)
          (buffer/get! ibuf n)
          (begin (fill) (sock/recv-exact n))))

    (define (sock/send-rope rope-data npad)
      (check-open)
      (send-encrypted-packet (tls-ctype:appdata) rope-data))

    (define (sock/send data)
      (sock/send-rope (rope:leaf data) 0))

    (define (sock/close)
      (when (eq? state (tlsc-state:cnctd))
        (send-alert (tls-alert-desc:close-notify))
        (set! state (tlsc-state:closed)))
      (sock.close))

    (define (sock/get-info)
      {suite  = (cipher-suite->name suite)
       group  = (named-group->name kex.group)
       sigalg = (sigalg->string sigalg)
       alpn   = alpn
       certs  = certs
       clean-close = clean-close})

    ;; --- handshake ---
    (try
     (begin
       (send-client-hello)
       (while (not (eq? state (tlsc-state:cnctd)))
         (read-records)))
     except
     (:TLS/Alert desc msg)
     -> (begin
          (send-alert desc)
          (set! failure msg))
     (:X509/Invalid msg)
     -> (begin
          (send-alert (tls-alert-desc:bad-cert))
          (set! failure msg))
     (:TLS/ReceivedFatalAlert desc)
     -> (set! failure (format "server sent alert '" (sym (tls-alert-desc->name desc)) "'"))
     (:Doom/EOF _)
     -> (set! failure "connection closed by server")
     (:OSError n)
     -> (set! failure (format "OS error " (int n)))
     )
    (when (not (eq? state (tlsc-state:cnctd)))
      (set! state (tlsc-state:closed))
      (sock.close)
      (raise (:TLS/ClientFailed failure)))

    {recv       = sock/recv
     recv-exact = sock/recv-exact
     send       = sock/send
     send-rope  = sock/send-rope
     close      = sock/close
     get-fd     = sock.get-fd
     get-info   = sock/get-info
     }
    ))

;; connect to <ip>:<port> and handshake, checking the server's
;;  certificates for <host> against <anchors>.
(define (tls13/connect ip port host alpns anchors)
  (let ((sock (doom/make (tcp4-sock)))
        (now (let ((t (gettime/realtime))) t.sec)))
    (sock.connect (address/make4 ip port))
    (tls13/client sock host alpns
                  (lambda (ders) (x509-verify-server ders anchors host now)))))
