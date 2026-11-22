;; -*- Mode: Irken -*-

;; lib/crypto/webauthn.scm: registration and assertion, ES256 and
;;  Ed25519, and that tampering with any part fails (with an error,
;;  never a crash).  the vectors were made with python's cryptography
;;  package, for rp id "localhost" and origin "https://localhost:4433".

(require "lib/basis.scm")
(require "lib/crypto/webauthn.scm")

(define rp "localhost")
(define origin "https://localhost:4433")

(define vectors
  (list
   (list "reg" "es256" "GiehN5bLj3q_chWTPDvzzD3bY4wtTnCdIhKPhimRV7s" "eyJ0eXBlIjogIndlYmF1dGhuLmNyZWF0ZSIsICJjaGFsbGVuZ2UiOiAiR2llaE41YkxqM3FfY2hXVFBEdnp6RDNiWTR3dFRuQ2RJaEtQaGltUlY3cyIsICJvcmlnaW4iOiAiaHR0cHM6Ly9sb2NhbGhvc3Q6NDQzMyIsICJjcm9zc09yaWdpbiI6IGZhbHNlfQ" "o2NmbXRkbm9uZWdhdHRTdG10oGhhdXRoRGF0YViUSZYN5YgOjGh0NBcPZHZgW4_krrmihjLHmVzzuoMdl2NFAAAAAAAAAAAAAAAAAAAAAAAAAAAAEOAzDFbsUaxZHlCvy59InTylAQIDJiABIVgg8Goug1Aaid--pzx4Qf88jez4ZJY51Az7jacH3ykENB4iWCDEmn8kvKfXZq5Jk0kkzKEn5MdEZ4zH2SrbMo-QTGfWCg" "4DMMVuxRrFkeUK_Ln0idPA")
   (list "get" "es256" "0BetQQg1sIVWAIuKlT_3oZgZuYprTHDTGUHBFWefivU" "eyJ0eXBlIjogIndlYmF1dGhuLmdldCIsICJjaGFsbGVuZ2UiOiAiMEJldFFRZzFzSVZXQUl1S2xUXzNvWmdadVlwclRIRFRHVUhCRldlZml2VSIsICJvcmlnaW4iOiAiaHR0cHM6Ly9sb2NhbGhvc3Q6NDQzMyIsICJjcm9zc09yaWdpbiI6IGZhbHNlLCAib3RoZXJfa2V5c19jYW5fYmVfYWRkZWRfaGVyZSI6ICJkbyBub3QgY29tcGFyZSBjbGllbnREYXRhSlNPTiBhZ2FpbnN0IGEgdGVtcGxhdGUuIFNlZSBodHRwczpcXC9cXC9nb28uZ2xcXC95YWJQZXgifQ" "SZYN5YgOjGh0NBcPZHZgW4_krrmihjLHmVzzuoMdl2MFAAAABw" "MEUCIQDKlOPnLXXgv8KUYaM-eUr9zodGw4ZnKNeBkha20E-gVgIgNemd9BKZpDimfUeKE4VyLbYV1h6_SCrcWyLBHRpTlwU")
   (list "reg" "ed25519" "Dg20nSpK_6w7r15Xlbtj2h53wbpoNo9rXwo-iHWaoKw" "eyJ0eXBlIjogIndlYmF1dGhuLmNyZWF0ZSIsICJjaGFsbGVuZ2UiOiAiRGcyMG5TcEtfNnc3cjE1WGxidGoyaDUzd2Jwb05vOXJYd28taUhXYW9LdyIsICJvcmlnaW4iOiAiaHR0cHM6Ly9sb2NhbGhvc3Q6NDQzMyIsICJjcm9zc09yaWdpbiI6IGZhbHNlfQ" "o2NmbXRkbm9uZWdhdHRTdG10oGhhdXRoRGF0YVhxSZYN5YgOjGh0NBcPZHZgW4_krrmihjLHmVzzuoMdl2NFAAAAAAAAAAAAAAAAAAAAAAAAAAAAEJlTHMBGzI0vuXESn6TNnjekAQEDJyAGIVggkyJGNR01KdEXr5uB-H-Ok7G2bfuZbrvH6Bd4EMh5u6Y" "mVMcwEbMjS-5cRKfpM2eNw")
   (list "get" "ed25519" "i6NWW9qWx5RzAZbFehexYJSb6UUFf6R1AE7e80BFRzM" "eyJ0eXBlIjogIndlYmF1dGhuLmdldCIsICJjaGFsbGVuZ2UiOiAiaTZOV1c5cVd4NVJ6QVpiRmVoZXhZSlNiNlVVRmY2UjFBRTdlODBCRlJ6TSIsICJvcmlnaW4iOiAiaHR0cHM6Ly9sb2NhbGhvc3Q6NDQzMyIsICJjcm9zc09yaWdpbiI6IGZhbHNlLCAib3RoZXJfa2V5c19jYW5fYmVfYWRkZWRfaGVyZSI6ICJkbyBub3QgY29tcGFyZSBjbGllbnREYXRhSlNPTiBhZ2FpbnN0IGEgdGVtcGxhdGUuIFNlZSBodHRwczpcXC9cXC9nb28uZ2xcXC95YWJQZXgifQ" "SZYN5YgOjGh0NBcPZHZgW4_krrmihjLHmVzzuoMdl2MFAAAABw" "caY2MVnI6hkQbonNkUH2tuJRxSoFZsjslXqoxQgXCJHwCTmhN9ls7dj18iUwxH1ZFnDC5jcHGFw9SFd7y0-dDw")))

(define (check name thunk expect-ok?)
  (let ((ok (try
             (begin (thunk) #t)
             except
             (:WebAuthn/Failed _) -> #f
             (:CBOR/Bad _ _) -> #f)))
    (printf (if (eq? ok expect-ok?) "ok   " "FAIL ") name "\n")))

;; <s> with bit <bit> of byte <i> flipped.
(define (flip-bit s i bit)
  (let ((r (copy-string s (string-length s))))
    (string-set! r i (int->char (logxor (<< 1 bit) (char->int (string-ref r i)))))
    r))

(define (flip s i) (flip-bit s i 0))

(define (same-key? a b)
  (match a b with
    (cose-key:es256 x) (cose-key:es256 y) -> (string=? x y)
    (cose-key:ed25519 x) (cose-key:ed25519 y) -> (string=? x y)
    _ _ -> #f))

(define (run)
  (let ((key (cose-key:ed25519 "")))
    (for-list v vectors
      (match v with
        ("reg" kind ch0 cdj0 ao0 id0)
        -> (let ((ch (b64url-decode ch0)) (cdj (b64url-decode cdj0)) (ao (b64url-decode ao0)) (id (b64url-decode id0)))
             (define (reg ch origin rp ao) (webauthn/verify-registration rp origin ch cdj ao))
             (check (format "reg " kind)
                    (lambda ()
                      (let ((c (reg ch origin rp ao)))
                        (assert (string=? c.id id))
                        ;; the key's COSE bytes (what's stored) give the same key back.
                        (assert (same-key? (webauthn/cose-key (cbor/decode c.raw)) c.key))
                        (set! key c.key)))
                    #t)
             (check (format "reg " kind " wrong challenge") (lambda () (reg (flip ch 0) origin rp ao)) #f)
             (check (format "reg " kind " wrong origin") (lambda () (reg ch "https://evil.example" rp ao)) #f)
             (check (format "reg " kind " wrong rp id") (lambda () (reg ch origin "example.com" ao)) #f)
             (check (format "reg " kind " truncated") (lambda () (reg ch origin rp (substring ao 0 (- (string-length ao) 5)))) #f)
             (for-range i (string-length ao)
               (for-range bit 8
                 (try (begin (reg ch origin rp (flip-bit ao i bit)) #u)
                      except (:WebAuthn/Failed _) -> #u (:CBOR/Bad _ _) -> #u)))
             (printf "ok   reg " kind " every bit flipped\n"))
        ("get" kind ch0 cdj0 ad0 sig0)
        -> (let ((ch (b64url-decode ch0)) (cdj (b64url-decode cdj0)) (ad (b64url-decode ad0)) (sig (b64url-decode sig0)))
             (define (get ch cdj ad sig) (webauthn/verify-assertion rp origin ch cdj ad sig key))
             (check (format "get " kind) (lambda () (assert (= 7 (get ch cdj ad sig)))) #t)
             (check (format "get " kind " bad signature") (lambda () (get ch cdj ad (flip sig 10))) #f)
             (check (format "get " kind " counter changed") (lambda () (get ch cdj (flip ad 36) sig)) #f)
             (check (format "get " kind " user not verified") (lambda () (get ch cdj (flip-bit ad 32 2) sig)) #f)
             (check (format "get " kind " user not present") (lambda () (get ch cdj (flip-bit ad 32 0) sig)) #f)
             (check (format "get " kind " other challenge") (lambda () (get (flip ch 3) cdj ad sig)) #f)
             (check (format "get " kind " clientData changed") (lambda () (get ch (flip cdj 5) ad sig)) #f))
        _ -> (impossible)))))

(run)
