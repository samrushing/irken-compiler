;; -*- Mode: Irken -*-

(require "lib/crypto/sig.scm")

;; rather than using pattern matching directly, think
;;  about using haskell-style 'monadic' parsing.  this might
;;  be better for 'regular-language' style stuff like optional
;;  parts.

(datatype public-key
  (:rsa big int)    ;; n e
  (:ed25519 string) ;; + sha512
  (:ed448 string)   ;; + shake256
  (:ec string (list int)) ;; key OID
  )

;; we have OIDs for key types, but then also OIDs for key+hash.
;; so we need datatypes for keys, hashes, and tables for their combination.
;; (define keytype->oid
;;   (alist/make
;;    ((keytype:rsa) '(1 2 840 113549 1 1 1))
;;    ...))

(define (parse-x509 asn1)

  (define (unexpected s x)
    (raise (:X509/Unexpected s x)))

  (define p-x509
    (ber:SEQUENCE
     (tbsCertificate
      signatureAlgorithm
      signatureValue))
    -> { tbs  = tbsCertificate ;; keep DER ('to be signed')
         cert = (p-tbsCertificate tbsCertificate)
         algo = (p-AlgorithmIdentifier signatureAlgorithm)
         sig  = (p-Signature signatureValue) }
    x -> (unexpected "Certificate" x)
    )

  (define p-Serial
    (ber:INTEGER serial)  -> (int->big serial)
    (ber:BINTEGER serial) -> (u256->big serial)
    x -> (unexpected "Serial" x)
    )

  (define p-AlgorithmIdentifier
    (ber:SEQUENCE ((ber:OID oid) . params)) -> {oid=oid params=params}
    x -> (unexpected "AlgorithmIdentifier" x)
    )

  (define p-Signature
    (ber:STRING (asn1string:BIT) bs) -> (substring bs 1 (string-length bs))
    x -> (unexpected "Signature" x)
    )

  (define p-AttributeTypeAndValue
    (ber:SEQUENCE ((ber:OID oid) value))
    -> {oid=oid value=value}
    x -> (unexpected "AttributeTypeAndValue" x)
    )

  (define p-RelativeDistinguishedName
    (ber:SET pairs)
    -> (map p-AttributeTypeAndValue pairs)
    x -> (unexpected "RelativeDistinguishedName" x)
    )

  (define p-Name
    (ber:SEQUENCE rdns)
    -> (map p-RelativeDistinguishedName rdns)
    x -> (unexpected "Name" x)
    )

  ;; Time ::= CHOICE {utcTime UTCTime, generalTime GeneralizedTime}
  ;;         UTCTime ::=?   YYMMDDHHMMSSZ
  ;; GeneralizedTime ::=? YYYYMMDDHHMMSSZ

  (define p-Time
    (ber:STRING (asn1string:UTCTIME) s)         -> s
    (ber:STRING (asn1string:GENERALIZEDTIME) s) -> s
    x -> (unexpected "Time" x)
    )

  (define p-Validity
    (ber:SEQUENCE (not-before not-after))
    -> {not-before = (p-Time not-before)
        not-after  = (p-Time not-after)}
    x -> (unexpected "Validity" x)
    )


  (define (p-encapsulated-bitstring bs)
    ;; note: bitstrings in this context should start with "\x00" to
    ;; indicate no padding bits are added.
    (let ((bslen (string-length bs))
          (p (string-emitter bs)))
      (match (p.byte) with
        0 -> (let (((asn1 len) (ber/decode p)))
               (when (not (= len (- bslen 1)))
                 (raise (:ASN1/ExtraData len (- bslen 1))))
               asn1)
        n -> (unexpected "padded BITSTRING" (ber:STRING (asn1string:BIT) bs))
        )))

  (define (p-RSAPublicKey bs)
    (match (p-encapsulated-bitstring bs) with
      (ber:SEQUENCE ((ber:BINTEGER n) (ber:INTEGER e)))
      -> (public-key:rsa (u256->big n) e)
      x -> (unexpected "RSAPublicKey" x)
      ))

  (define (p-Ed25519PublicKey bs)
    (match (string-ref bs 0) (string-length bs) with
      ;; rfc8032 5.1.5 - 32 little-endian octets
      #\nul 33 -> (public-key:ed25519 (substring bs 1 33))
      _ _      -> (unexpected "Ed25519PublicKey" (ber:STRING (asn1string:BIT) bs))
      ))

  (define (p-Ed448PublicKey bs)
    (match (string-ref bs 0) (string-length bs) with
      ;; rfc8032 5.2.5 - 57 little-endian octets
      #\nul 58 -> (public-key:ed448 (substring bs 1 58))
      _ _      -> (unexpected "Ed448PublicKey" (ber:STRING (asn1string:BIT) bs))
      ))

  (define (p-ECPublicKey bs params)
    (match (string-ref bs 0) params with
      ;; XXX we could do length checks depending on the curve
      #\nul ((ber:OID curve))
      -> (public-key:ec (substring bs 1 (string-length bs)) curve)
      _ _ -> (unexpected "ECPublicKey" (ber:STRING (asn1string:BIT) bs))
      ))

  (define (p-PublicKey algo s)
    (match algo.oid with
      (1 2 840 113549 1 1 1) -> (p-RSAPublicKey s)
      (1 2 840 10045 2 1)    -> (p-ECPublicKey s algo.params)
      (1 3 101 112)          -> (p-Ed25519PublicKey s)
      (1 3 101 113)          -> (p-Ed448PublicKey s)
      x                      -> (raise (:X509/Unsupported "Public Key" x))
      ))

  (define p-SubjectPublicKeyInfo
    (ber:SEQUENCE (algo0 (ber:STRING (asn1string:BIT) key)))
    -> (let ((algo1 (p-AlgorithmIdentifier algo0)))
         {algo=algo1 key=(p-PublicKey algo1 key)})
    x -> (unexpected "SubjectPublicKeyInfo" x)
    )

  (define p-tbsCertificate*
    (serial-number ;; might be either INTEGER or BINTEGER, depending on age.
     signature-kind
     issuer
     validity
     subject
     subjectPublicKeyInfo
     . extras)
    -> {version  = 1
        serial   = (p-Serial serial-number)
        sig-kind = (p-AlgorithmIdentifier signature-kind)
        issuer   = (p-Name issuer)
        validity = (p-Validity validity)
        subject  = (p-Name subject)
        pubkey   = (p-SubjectPublicKeyInfo subjectPublicKeyInfo)
        extras   = extras
        }
    x -> (unexpected "tbsCertificate" (ber:SEQUENCE x))
    )

  ;; this is the only point where I'm tempted to use a 'real' asn1
  ;; parser generator.  but then I'd be tempted to write it. 8^)

  (define p-tbsCertificate
    (ber:SEQUENCE
     ((ber:CONTEXT 0 ((ber:INTEGER version))) . rest))
    -> (let ((cert (p-tbsCertificate* rest)))
         (set! cert.version (+ 1 version))
         cert)
    (ber:SEQUENCE rest) ;; no version, defaults to 1
    -> (p-tbsCertificate* rest)
    x -> (unexpected "tbsCertificate" x)
    )

  (p-x509 asn1)
  )

(define (read-certificate path)
  (match (read-pem-asn1 path) with
    (:tuple "CERTIFICATE" asn1)
    -> asn1
    (:tuple kind _)
    -> (raise (:SIG/UnexpectedPEM "CERTIFICATE" kind))
    ))

(define (oid-repr oid)
  (format (join int->string "." oid)))

(define (name-repr names)
  (define (pair-repr pair)
    (format (rpad 10 (oid-repr pair.oid)) ":" (ber-repr pair.value)))
  (let ((result '()))
    (for-list parts names
      (push! result (format (join pair-repr ", " parts))))
    (format "\n  : " (join "\n  : " (reverse result)))))

;; we actually need a whole mess of parsers/printers for
;;  the various x509v3 extensions.
(define print-extras
  ()    -> #u
  (iid . rest)
  -> (begin
       (printf (bold "extras") " {")
       (pp-ber iid 1)
       (for-list item rest
         (pp-ber iid 4))
       (printf "\n}\n")))

(define print-pubkey
  (public-key:rsa n e)
  -> (printf (bold "RSA public key") "\n"
             "    n = " (big->dec n) "\n"
             "    e = " (int e) "\n")
  (public-key:ed25519 k)
  -> (printf (bold "Ed25519 public key") "\n"
             "    k = " (string->hex k) "\n")
  (public-key:ed448 k)
  -> (printf (bold "Ed448 public key") "\n"
             "    k = " (string->hex k) "\n")
  (public-key:ec k oid)
  -> (printf (bold "ECDSA public key") "\n"
             "    k = " (string->hex k) "\n"
             "  oid = " (join int->string "." oid) "\n")
  )

(define (verify-cert x509)
  (let ((c (collector))
        (_ (ber/encode c.push x509.tbs))
        (der (c.val)))
    (match x509.cert.pubkey.key with
      (public-key:rsa n e)
      -> (printf "verify: " (bool (rsa-verify n (int->big e) der x509.sig)) "\n")
      (public-key:ed25519 pk)
      -> (printf "verify: " (bool (ed25519-verify x509.sig der pk)) "\n")
      _ -> (printf "verify: -- key type not supported --\n")
      )))

(define (print-cert cert)
  (printf
   (bold "version  ") (int cert.version) "\n"
   (bold "serial   ") (big-repr cert.serial) "\n"
   (bold "sigkind  ") (oid-repr cert.sig-kind.oid) "\n" ;; XXX params
   (bold "issuer   ") (name-repr cert.issuer) "\n"
   (bold "validity ") cert.validity.not-before "-" cert.validity.not-after "\n"
   (bold "subject  ") (name-repr cert.subject) "\n"
   )
  (print-pubkey cert.pubkey.key)
  (print-extras cert.extras)
  )
