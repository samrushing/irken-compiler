;; -*- Mode: Irken -*-

;; --- record layer ---

(make-enum tls-ctype
  (invalid             0)
  (change-cipher-spec 20)
  (alert              21)
  (hsk                22)
  (appdata            23)
  )

(datatype tls-record
  (:alert tls-alert-level tls-alert-desc)
  (:hsk tls-hsk)
  (:appdata string)
  (:change-cipher-spec)
  (:other int string)
  )

;; --- alerts ---

(make-enum tls-alert-level
  (warning 1)
  (fatal   2)
  )

(make-enum tls-alert-desc
  (close-notify             0)
  (unexpected-message       10)
  (bad-record-mac           20)
  (record-overflow          22)
  (hsk-failure              40)
  (bad-cert                 42)
  (unsupported-cert         43)
  (cert-revoked             44)
  (cert-expired             45)
  (cert-unknown             46)
  (illegal-parameter        47)
  (unknown-ca               48)
  (access-denied            49)
  (decode-error             50)
  (decrypt-error            51)
  (protocol-version         70)
  (insufficient-security    71)
  (internal-error           80)
  (inappropriate-fallback   86)
  (user-canceled            90)
  (missing-extension        109)
  (unsupported-extension    110)
  (unrecognized-name        112)
  (bad-cert-status-response 113)
  (unknown-psk-identity     115)
  (cert-required            116)
  (no-application-protocol  120)
  )

;; --- handshake protocol ---

(make-enum tls-hsk-type
  (client-hello       1)
  (server-hello       2)
  (new-session-ticket 4)
  (end-of-early-data  5)
  (enc-exts           8)
  (cert               11)
  (cert-request       13)
  (cert-verify        15)
  (finished           20)
  (key-update         24)
  (message-hash       254)
  )

(datatype tls-hsk
  (:client-hello {legver=int random=string sessid=string suites=(list int) exts=(list tlsext)})
  (:server-hello {legver=int random=string sessid=string suite=int exts=(list tlsext)})
  (:finished string) ;; verify-data
  (:other string)
  )

;; --- extensions ---

(make-enum tlsext-type
  (server-name            0)  ;; RFC 6066
  (maxfraglen             1)  ;; RFC 6066
  (status-request         5)  ;; RFC 6066
  (supported-groups       10) ;; RFC 4492, 7919
  (sigalgs                13) ;; [[this document]]
  (use-srtp               14) ;; RFC 5764
  (heartbeat              15) ;; RFC 6520
  (alpn                   16) ;; RFC 7301
  (signed-cert-timestamp  18) ;; RFC 6962
  (client-cert-type       19) ;; RFC 7250
  (server-cert-type       20) ;; RFC 7250
  (padding                21) ;; RFC 7685
  (pre-shared-key         41) ;; [[this document]]
  (early-data             42) ;; [[this document]]
  (supported-versions     43) ;; [[this document]]
  (cookie                 44) ;; [[this document]]
  (psk-kex-modes          45) ;; [[this document]]
  (cert-authorities       47) ;; [[this document]]
  (oid-filters            48) ;; [[this document]]
  (post-hsk-auth          49) ;; [[this document]]
  (sigalgs-cert           50) ;; [[this document]]
  (key-share              51) ;; [[this document]]
  )

(datatype tlsext
  (:server-names (list string))
  (:supported-groups (list int))
  (:sigalgs (list int))
  (:supported-versions (list int))
  (:psk-kex-modes (list int))
  (:sigalgs-cert (list int))
  (:client-shares (list {group=int kex=string}))
  (:key-share {group=int kex=string})
  (:hrr-key-share int)
  (:padding int)
  (:alpn (list string))
  (:maxfraglen int) ;; this could be an enum to clamp to 1..4
  (:other int string)
  )

(define tlsext->name
  (tlsext:server-names _)       -> 'server-names
  (tlsext:supported-groups _)   -> 'supported-groups
  (tlsext:sigalgs _)            -> 'sigalgs
  (tlsext:supported-versions _) -> 'supported-versions
  (tlsext:psk-kex-modes _)      -> 'psk-kex-modes
  (tlsext:sigalgs-cert _)       -> 'sigalgs-cert
  (tlsext:client-shares _)      -> 'client-shares
  (tlsext:key-share _)          -> 'key-share
  (tlsext:hrr-key-share _)      -> 'hrr-key-share
  (tlsext:padding _)            -> 'padding
  (tlsext:alpn _)               -> 'alpn
  (tlsext:maxfraglen _)         -> 'maxfraglen
  (tlsext:other _ _)            -> 'other
  )

(define find-extension
  name ()
  -> (maybe:no)
  name (ext . exts)
  -> (if (eq? name (tlsext->name ext))
         (maybe:yes ext)
         (find-extension name exts)))

;; --- signature algorithms ---

(make-enum sigalg
  (rsa-pkcs1-sha256       #x0401)
  (rsa-pkcs1-sha384       #x0501)
  (rsa-pkcs1-sha512       #x0601)
  (ecdsa-secp256r1-sha256 #x0403)
  (ecdsa-secp384r1-sha384 #x0503)
  (ecdsa-secp521r1-sha512 #x0603)
  (rsa-pss-rsae-sha256    #x0804)
  (rsa-pss-rsae-sha384    #x0805)
  (rsa-pss-rsae-sha512    #x0806)
  (ed25519                #x0807)
  (ed448                  #x0808)
  (rsa-pss-pss-sha256     #x0809)
  (rsa-pss-pss-sha384     #x080a)
  (rsa-pss-pss-sha512     #x080b)
  (rsa-pkcs1-sha1         #x0201)
  (ecdsa-sha1             #x0203)
  )

(define (sigalg->string alg)
  (match (alist/lookup sigalg-rev-alist alg) with
    (maybe:yes name) -> (format (sym (sigalg->name name)))
    (maybe:no)       -> (format "unknown-sigalg-x" (hex alg))
    ))

;; --- cipher suites ---

(make-enum cipher-suite
  (aes-128-gcm-sha256       #x1301)
  (aes-256-gcm-sha384       #x1302)
  (chacha20-poly1305-sha256 #x1303)
  (aes-128-ccm-sha256       #x1304)
  (aes-128-ccm-8-sha256     #x1305)
  )

(define (suite->string val)
  (match (alist/lookup cipher-suite-rev-alist val) with
    (maybe:yes name) -> (format (sym (cipher-suite->name name)))
    (maybe:no)       -> (format "unknown-suite-x" (zpad 4 (hex val)))
    ))

;; --- key exchange ---

(make-enum named-group
  ;; Elliptic Curve Groups (ECDHE)
  (secp256r1 #x0017)
  (secp384r1 #x0018)
  (secp521r1 #x0019)
  (x25519    #x001D)
  (x448      #x001E)
  ;; Finite Field Groups (DHE)
  (ffdhe2048 #x0100)
  (ffdhe3072 #x0101)
  (ffdhe4096 #x0102)
  (ffdhe6144 #x0103)
  (ffdhe8192 #x0104)
  )

(define (named-group->string g)
  (match (alist/lookup named-group-rev-alist g) with
    (maybe:yes name) -> (format (sym (named-group->name name)))
    (maybe:no)       -> (format "unknown-group-x" (hex g))
    ))

;; --- state machine ---

(datatype tls-state
  (:start)
  (:rcvch)
  (:negot)
  (:wfini)
  (:cnctd)
  (:closed)
  )

(define tls-state->name
  (tls-state:start)  -> 'start
  (tls-state:rcvch)  -> 'received-client-hello
  (tls-state:negot)  -> 'negotiated
  (tls-state:wfini)  -> 'wait-finished
  (tls-state:cnctd)  -> 'connected
  (tls-state:closed) -> 'closed
  )
