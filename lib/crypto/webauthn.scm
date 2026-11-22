;; -*- Mode: Irken -*-

;; WebAuthn (passkey) verification for a relying party: checking a new
;;  credential (registration) and a login (assertion).
;;
;; https://www.w3.org/TR/webauthn-3/ sections 7.1 and 7.2.  attestation
;;  isn't verified (we ask for "none"): a credential is trusted because
;;  whoever runs the server chose to add it, not because of who made the
;;  authenticator.  keys: ES256 (P-256) and EdDSA (Ed25519), which is
;;  what passkeys use.
;;
;; everything that fails raises (:WebAuthn/Failed <why>), or CBOR/Bad
;;  for a malformed attestation object or key.

(require "lib/cbor.scm")
(require "lib/codecs/base64.scm")
(require "lib/crypto/sodium.scm")
(require "lib/crypto/ecdsa.scm")

(define (webauthn-fail why) (raise (:WebAuthn/Failed why)))

;; --------------------------------------------------------------------------------
;; clientDataJSON: a small, strict JSON reader.  we only need the
;;  top-level string and boolean members of an object; anything else is
;;  skipped (but must still be well-formed).  (a server can use it for
;;  the JSON its own pages send, too.)

(datatype wajson
  (:str string)
  (:bool bool)
  (:other)
  )

(define (webauthn/parse-json s)
  (let ((pos 0)
        (len (string-length s)))

    (define (bad) (webauthn-fail "malformed clientDataJSON"))
    (define (peek) (if (< pos len) (string-ref s pos) (bad)))
    (define (next!) (let ((c (peek))) (inc! pos) c))
    (define (skip-ws)
      (while (and (< pos len) (member? (string-ref s pos) '(#\space #\tab #\newline #\return) eq?))
        (inc! pos)))
    (define (expect ch)
      (skip-ws)
      (if (eq? (next!) ch) #u (bad)))
    (define (expect-word word)
      (for-string ch word
        (if (eq? (next!) ch) #u (bad))))

    (define (hex1 ch)
      (let ((n (char->int ch)))
        (cond ((and (>= n 48) (<= n 57)) (- n 48))
              ((and (>= n 97) (<= n 102)) (- n 87))
              ((and (>= n 65) (<= n 70)) (- n 55))
              (else (bad)))))

    ;; code point <c> as UTF-8.
    (define (utf8 c)
      (define (b n) (int->char n))
      (cond ((< c #x80) (list (b c)))
            ((< c #x800) (list (b (logior #xc0 (>> c 6))) (b (logior #x80 (logand c #x3f)))))
            (else (list (b (logior #xe0 (>> c 12)))
                        (b (logior #x80 (logand (>> c 6) #x3f)))
                        (b (logior #x80 (logand c #x3f)))))))

    (define (string-value)
      (expect #\")
      (let ((out (list:nil)))
        (let loop ()
          (let ((ch (next!)))
            (cond ((eq? ch #\") #u)
                  ((< (char->int ch) 32) (bad))
                  ((eq? ch #\\)
                   (match (next!) with
                     #\" -> (push! out #\")
                     #\\ -> (push! out #\\)
                     #\/ -> (push! out #\/)
                     #\b -> (push! out (int->char 8))
                     #\f -> (push! out (int->char 12))
                     #\n -> (push! out #\newline)
                     #\r -> (push! out #\return)
                     #\t -> (push! out #\tab)
                     #\u -> (let ((c 0))
                              (for-range i 4 (set! c (logior (<< c 4) (hex1 (next!)))))
                              (for-list b (utf8 c) (push! out b)))
                     _ -> (bad))
                   (loop))
                  (else (push! out ch) (loop)))))
        (list->string (reverse out))))

    ;; any value, at nesting <depth>.
    (define (value depth)
      (when (> depth 8) (bad))
      (skip-ws)
      (match (peek) with
        #\" -> (wajson:str (string-value))
        #\t -> (begin (expect-word "true") (wajson:bool #t))
        #\f -> (begin (expect-word "false") (wajson:bool #f))
        #\n -> (begin (expect-word "null") (wajson:other))
        #\{ -> (begin (members depth) (wajson:other))
        #\[ -> (begin
                 (next!)
                 (skip-ws)
                 (if (eq? (peek) #\])
                     (next!)
                     (let loop ()
                       (value (+ depth 1))
                       (skip-ws)
                       (match (next!) with
                         #\, -> (loop)
                         #\] -> #\]
                         _ -> (bad))))
                 (wajson:other))
        _ -> (begin
               ;; a number: digits, sign, point, exponent.
               (let ((start pos))
                 (while (and (< pos len)
                             (member? (string-ref s pos) (string->list "0123456789+-.eE") eq?))
                   (inc! pos))
                 (if (= pos start) (bad) (wajson:other))))))

    ;; an object's members, as (:tuple key value)s.
    (define (members depth)
      (expect #\{)
      (skip-ws)
      (let ((r (list:nil)))
        (if (eq? (peek) #\})
            (next!)
            (let loop ()
              (skip-ws)
              (let ((k (string-value)))
                (expect #\:)
                (push! r (:tuple k (value (+ depth 1))))
                (skip-ws)
                (match (next!) with
                  #\, -> (loop)
                  #\} -> #\}
                  _ -> (bad)))))
        (reverse r)))

    (let ((r (members 0)))
      (skip-ws)
      (if (= pos len) r (bad)))))

(define (webauthn/json-get members key)
  (match members with
    () -> (wajson:other)
    ((:tuple k v) . rest) -> (if (string=? k key) v (webauthn/json-get rest key))))

;; string member <key>, or "".
(define (webauthn/json-string members key)
  (match (webauthn/json-get members key) with
    (wajson:str s) -> s
    _ -> ""))

;; the challenge in clientDataJSON <cdj> (decoded): so a server can
;;  find which of the challenges it handed out this answers.
(define (webauthn/client-challenge cdj)
  (b64url-decode (webauthn/json-string (webauthn/parse-json cdj) "challenge")))

;; check clientDataJSON <cdj>: its type, challenge and origin.
(define (webauthn-check-client-data cdj type challenge origin)
  (let ((cd (webauthn/parse-json cdj)))
    (match (webauthn/json-get cd "type") with
      (wajson:str t) -> (when (not (string=? t type)) (webauthn-fail (format "type is " t)))
      _ -> (webauthn-fail "no type"))
    (match (webauthn/json-get cd "challenge") with
      ;; not secret: plain comparison is fine.
      (wajson:str c) -> (when (not (string=? (b64url-decode c) challenge)) (webauthn-fail "wrong challenge"))
      _ -> (webauthn-fail "no challenge"))
    (match (webauthn/json-get cd "origin") with
      (wajson:str o) -> (when (not (string=? o origin)) (webauthn-fail (format "wrong origin: " o)))
      _ -> (webauthn-fail "no origin"))
    (match (webauthn/json-get cd "crossOrigin") with
      (wajson:bool #t) -> (webauthn-fail "cross-origin")
      _ -> #u)))

;; --------------------------------------------------------------------------------
;; authenticator data (section 6.1)

(define WEBAUTHN-UP #x01) ;; user present
(define WEBAUTHN-UV #x04) ;; user verified (PIN, biometric)
(define WEBAUTHN-AT #x40) ;; attested credential data included
(define WEBAUTHN-ED #x80) ;; extensions included

(datatype cose-key
  (:es256 string)   ;; an uncompressed point: 04 x y
  (:ed25519 string) ;; 32 bytes
  )

;; a COSE_Key (RFC 9052 section 7, RFC 9053) as a cose-key.
(define (webauthn/cose-key k)
  (define (int-field n)
    (match (cbor/get-int k n) with
      (maybe:yes (cbor:uint v)) -> (maybe:yes v)
      (maybe:yes (cbor:nint v)) -> (maybe:yes v)
      _ -> (maybe:no)))
  (define (bytes-field n)
    (match (cbor/get-int k n) with
      (maybe:yes (cbor:bytes b)) -> (if (= (string-length b) 32) b (webauthn-fail "bad key size"))
      _ -> (webauthn-fail "key without coordinates")))
  ;; kty=1, alg=3, crv=-1, x=-2, y=-3
  (match (int-field 1) (int-field 3) (int-field -1) with
    (maybe:yes 2) (maybe:yes -7) (maybe:yes 1)
    -> (cose-key:es256 (format "\x04" (bytes-field -2) (bytes-field -3)))
    (maybe:yes 1) (maybe:yes -8) (maybe:yes 6)
    -> (cose-key:ed25519 (bytes-field -2))
    _ _ _ -> (webauthn-fail "unsupported key type (only ES256 and Ed25519)")))

;; parse authenticator data <ad>, checking it's for <rp-id> and that the
;;  user was present and verified.  returns {sign-count cred} where cred
;;  is (maybe {id key raw}) - <raw> the key's COSE bytes.
(define (webauthn/parse-auth-data ad rp-id)
  (let ((len (string-length ad)))
    (when (< len 37) (webauthn-fail "short authenticator data"))
    (when (not (string=? (substring ad 0 32) (sha256 rp-id)))
      (webauthn-fail "authenticator data is for another site"))
    (let ((flags (char->int (string-ref ad 32)))
          (count (logior* (<< (char->int (string-ref ad 33)) 24)
                          (<< (char->int (string-ref ad 34)) 16)
                          (<< (char->int (string-ref ad 35)) 8)
                          (char->int (string-ref ad 36))))
          (pos 37)
          (cred (maybe:no)))
      (when (= 0 (logand flags WEBAUTHN-UP)) (webauthn-fail "user not present"))
      (when (= 0 (logand flags WEBAUTHN-UV)) (webauthn-fail "user not verified"))
      (when (> (logand flags WEBAUTHN-AT) 0)
        (when (< len (+ pos 18)) (webauthn-fail "short attested credential data"))
        ;; aaguid (16), then the credential id's length (2)
        (let ((idlen (logior (<< (char->int (string-ref ad (+ pos 16))) 8)
                             (char->int (string-ref ad (+ pos 17)))))
              (idpos (+ pos 18)))
          (when (or (> idlen 1023) (> (+ idpos idlen) len)) (webauthn-fail "bad credential id length"))
          (let ((id (substring ad idpos (+ idpos idlen)))
                (kpos (+ idpos idlen))
                ((k kend) (cbor/decode-at ad kpos)))
            (set! cred (maybe:yes {id=id key=(webauthn/cose-key k) raw=(substring ad kpos kend)}))
            (set! pos kend))))
      (when (> (logand flags WEBAUTHN-ED) 0)
        (let (((_ end) (cbor/decode-at ad pos)))
          (set! pos end)))
      (when (not (= pos len)) (webauthn-fail "trailing bytes in authenticator data"))
      {sign-count=count cred=cred})))

;; --------------------------------------------------------------------------------
;; registration (7.1): navigator.credentials.create().
;;
;; <cdj> is clientDataJSON, <ao> the attestationObject (both decoded
;;  from base64url).  returns {id key raw sign-count}.
(define (webauthn/verify-registration rp-id origin challenge cdj ao)
  (webauthn-check-client-data cdj "webauthn.create" challenge origin)
  (let ((obj (cbor/decode ao)))
    (match (cbor/get-text obj "authData") with
      (maybe:yes (cbor:bytes ad))
      -> (let ((parsed (webauthn/parse-auth-data ad rp-id)))
           (match parsed.cred with
             (maybe:yes c) -> {id=c.id key=c.key raw=c.raw sign-count=parsed.sign-count}
             (maybe:no) -> (webauthn-fail "no credential in the attestation")))
      _ -> (webauthn-fail "no authData in the attestation"))))

;; --------------------------------------------------------------------------------
;; authentication (7.2): navigator.credentials.get().
;;
;; check the assertion (<cdj> clientDataJSON, <ad> authenticatorData,
;;  <sig> signature) against the credential's key <key>.  returns the
;;  signature counter.
(define (webauthn/verify-assertion rp-id origin challenge cdj ad sig key)
  (webauthn-check-client-data cdj "webauthn.get" challenge origin)
  (let ((parsed (webauthn/parse-auth-data ad rp-id))
        (signed (string-concat (list ad (sha256 cdj)))))
    (when (not (match key with
                 (cose-key:es256 point) -> (ecdsa-verify ec-curve-p256 (sha256 signed) sig point)
                 (cose-key:ed25519 pk)  -> (and (= (string-length sig) 64) (ed25519-verify sig signed pk))))
      (webauthn-fail "bad signature"))
    parsed.sign-count))
