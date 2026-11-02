;; -*- Mode: Irken -*-

;; just enough JOSE for ACME (rfc8555 6.2): P-256 keys as JWKs
;;  (rfc7517), their thumbprints (rfc7638), and ES256 signatures in
;;  the flattened JSON serialization of JWS (rfc7515 7.2.2).
;;
;; requires lib/json.scm (and its parser libraries).

(require "lib/codecs/base64.scm")
(require "lib/crypto/sodium.scm")
(require "lib/crypto/p256.scm")

;; the JWK for a P-256 public key (04 || x || y).  the members are in
;;  lexicographic order, as a thumbprint requires.
(define (jwk-p256 pub)
  (json:obj
   (list (:tuple "crv" (json:string "P-256"))
         (:tuple "kty" (json:string "EC"))
         (:tuple "x" (json:string (b64url-encode (substring pub 1 33))))
         (:tuple "y" (json:string (b64url-encode (substring pub 33 65)))))))

;; rfc7638: base64url(sha256(the JWK's required members, compactly)).
(define (jwk-thumbprint jwk)
  (b64url-encode (sha256 (json->string jwk))))

;; sign <payload> (a string, possibly empty) with the P-256 key <sk>,
;;  under the protected header <protected> (a json:obj, which this
;;  adds "alg" to).  returns the flattened JWS, as a json:obj.
(define (jws-es256 sk RNG protected payload)
  (let ((header (match protected with
                  (json:obj pairs) -> (json:obj (list:cons (:tuple "alg" (json:string "ES256")) pairs))
                  _ -> (raise (:JOSE/BadHeader protected))))
        (p64 (b64url-encode (json->string header)))
        (b64 (b64url-encode payload))
        (sig (p256-sign-raw (format p64 "." b64) sk RNG)))
    (json:obj
     (list (:tuple "protected" (json:string p64))
           (:tuple "payload" (json:string b64))
           (:tuple "signature" (json:string (b64url-encode sig)))))))
