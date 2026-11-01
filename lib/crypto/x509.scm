;; -*- Mode: Irken -*-

(require "lib/crypto/sig.scm")
(require "lib/crypto/ecdsa.scm")
(require "lib/calendar.scm")

;; -- rfc5280 --

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

;; Name ::= SEQUENCE OF RelativeDistinguishedName
;; RelativeDistinguishedName ::= SET OF AttributeTypeAndValue
;; AttributeTypeAndValue ::= SEQUENCE { type OID, value ANY }
(define (x509-parse-name asn1)

  (define p-AttributeTypeAndValue
    (ber:SEQUENCE ((ber:OID oid) value))
    -> {oid=oid value=value}
    x -> (raise (:X509/Unexpected "AttributeTypeAndValue" x))
    )

  (define p-RelativeDistinguishedName
    (ber:SET pairs)
    -> (map p-AttributeTypeAndValue pairs)
    x -> (raise (:X509/Unexpected "RelativeDistinguishedName" x))
    )

  (match asn1 with
    (ber:SEQUENCE rdns)
    -> (map p-RelativeDistinguishedName rdns)
    x -> (raise (:X509/Unexpected "Name" x))
    ))

;; <der> is the DER encoding of a Certificate.
(define (parse-x509 der)

  (define (unexpected s x)
    (raise (:X509/Unexpected s x)))

  (define p-x509
    (ber:SEQUENCE
     (tbsCertificate
      signatureAlgorithm
      signatureValue))
    -> { der  = der
         tbs  = (first (ber/elements der)) ;; the DER that was signed, as received
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

  (define p-Name x509-parse-name)

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

  ;; Extension ::= SEQUENCE {
  ;;   extnID    OBJECT IDENTIFIER,
  ;;   critical  BOOLEAN DEFAULT FALSE,
  ;;   extnValue OCTET STRING -- the DER of a value whose type depends on extnID
  ;;   }
  (define p-Extension
    (ber:SEQUENCE ((ber:OID oid) (ber:BOOLEAN crit) (ber:STRING (asn1string:OCTET) v)))
    -> {oid=oid critical=crit value=v}
    (ber:SEQUENCE ((ber:OID oid) (ber:STRING (asn1string:OCTET) v)))
    -> {oid=oid critical=#f value=v}
    x -> (unexpected "Extension" x)
    )

  ;; the optional fields after subjectPublicKeyInfo are
  ;;  issuerUniqueID [1], subjectUniqueID [2], extensions [3].
  (define (p-Extensions extras)
    (let ((r (list:nil)))
      (for-list item extras
        (match item with
          (ber:CONTEXT 3 ((ber:SEQUENCE exts)))
          -> (set! r (map p-Extension exts))
          (ber:CONTEXT 3 x) -> (unexpected "Extensions" (ber:SEQUENCE x))
          _ -> #u))
      r))

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
        exts     = (p-Extensions extras)
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

  (p-x509 (ber->asn1 der))
  )

;; the DER of every certificate in the file at <path>: either PEM
;;  (any number of CERTIFICATE sections; others are skipped) or a
;;  single binary DER certificate.
(define (read-certificates path)
  (let ((pem (list:nil)))
    (with-file file (file/open-read path)
      (for section (pem-read-gen (file-char-generator file))
        (match section with
          (:tuple "CERTIFICATE" der) -> (push! pem der)
          _ -> #u)))
    (if (null? pem)
        (let ((parts (list:nil)))
          (with-file file (file/open-read path)
            (let loop ()
              (let ((s (file/read-buffer file)))
                (when (> (string-length s) 0)
                  (push! parts s)
                  (loop)))))
          (let ((der (string-concat (reverse parts))))
            (if (and (> (string-length der) 0) (= #x30 (char->int (string-ref der 0))))
                (list der)
                (raise (:SIG/NoPEMFound path)))))
        (reverse pem))))

;; the DER of the first certificate in the PEM file at <path>.
(define (read-certificate path)
  (with-file file (file/open-read path)
    (match ((pem-read-gen (file-char-generator file))) with
      (maybe:yes (:tuple "CERTIFICATE" der)) -> der
      (maybe:yes (:tuple kind _)) -> (raise (:SIG/UnexpectedPEM kind))
      (maybe:no) -> (raise (:SIG/NoPEMFound path))
      )))

;; --------------------------------------------------------------------------------
;; names for OIDs
;; --------------------------------------------------------------------------------

(define (oid-repr oid)
  (format (join int->string "." oid)))

(define oid=?
  () ()             -> #t
  (a . as) (b . bs) -> (and (= a b) (oid=? as bs))
  _ _               -> #f
  )

;; short names for the attribute types in a Name.
(define attr-name
  (2 5 4 3)  -> (maybe:yes "CN")
  (2 5 4 4)  -> (maybe:yes "SN")
  (2 5 4 5)  -> (maybe:yes "serialNumber")
  (2 5 4 6)  -> (maybe:yes "C")
  (2 5 4 7)  -> (maybe:yes "L")
  (2 5 4 8)  -> (maybe:yes "ST")
  (2 5 4 9)  -> (maybe:yes "street")
  (2 5 4 10) -> (maybe:yes "O")
  (2 5 4 11) -> (maybe:yes "OU")
  (2 5 4 12) -> (maybe:yes "title")
  (2 5 4 15) -> (maybe:yes "businessCategory")
  (2 5 4 17) -> (maybe:yes "postalCode")
  (2 5 4 42) -> (maybe:yes "GN")
  (2 5 4 97) -> (maybe:yes "organizationIdentifier")
  (1 2 840 113549 1 9 1)          -> (maybe:yes "emailAddress")
  (0 9 2342 19200300 100 1 1)     -> (maybe:yes "UID")
  (0 9 2342 19200300 100 1 25)    -> (maybe:yes "DC")
  (1 3 6 1 4 1 311 60 2 1 1)      -> (maybe:yes "jurisdictionL")
  (1 3 6 1 4 1 311 60 2 1 2)      -> (maybe:yes "jurisdictionST")
  (1 3 6 1 4 1 311 60 2 1 3)      -> (maybe:yes "jurisdictionC")
  _ -> (maybe:no)
  )

;; names for everything else.
(define oid-name
  ;; public key algorithms
  (1 2 840 113549 1 1 1)  -> (maybe:yes "rsaEncryption")
  (1 2 840 10045 2 1)     -> (maybe:yes "ecPublicKey")
  (1 3 101 110)           -> (maybe:yes "X25519")
  (1 3 101 111)           -> (maybe:yes "X448")
  (1 3 101 112)           -> (maybe:yes "Ed25519")
  (1 3 101 113)           -> (maybe:yes "Ed448")
  ;; signature algorithms
  (1 2 840 113549 1 1 5)  -> (maybe:yes "sha1WithRSAEncryption")
  (1 2 840 113549 1 1 10) -> (maybe:yes "RSASSA-PSS")
  (1 2 840 113549 1 1 11) -> (maybe:yes "sha256WithRSAEncryption")
  (1 2 840 113549 1 1 12) -> (maybe:yes "sha384WithRSAEncryption")
  (1 2 840 113549 1 1 13) -> (maybe:yes "sha512WithRSAEncryption")
  (1 2 840 10045 4 1)     -> (maybe:yes "ecdsa-with-SHA1")
  (1 2 840 10045 4 3 2)   -> (maybe:yes "ecdsa-with-SHA256")
  (1 2 840 10045 4 3 3)   -> (maybe:yes "ecdsa-with-SHA384")
  (1 2 840 10045 4 3 4)   -> (maybe:yes "ecdsa-with-SHA512")
  ;; curves
  (1 2 840 10045 3 1 7)   -> (maybe:yes "P-256")
  (1 3 132 0 34)          -> (maybe:yes "P-384")
  (1 3 132 0 35)          -> (maybe:yes "P-521")
  (1 3 132 0 10)          -> (maybe:yes "secp256k1")
  ;; extensions
  (2 5 29 9)              -> (maybe:yes "subjectDirectoryAttributes")
  (2 5 29 14)             -> (maybe:yes "subjectKeyIdentifier")
  (2 5 29 15)             -> (maybe:yes "keyUsage")
  (2 5 29 16)             -> (maybe:yes "privateKeyUsagePeriod")
  (2 5 29 17)             -> (maybe:yes "subjectAltName")
  (2 5 29 18)             -> (maybe:yes "issuerAltName")
  (2 5 29 19)             -> (maybe:yes "basicConstraints")
  (2 5 29 30)             -> (maybe:yes "nameConstraints")
  (2 5 29 31)             -> (maybe:yes "cRLDistributionPoints")
  (2 5 29 32)             -> (maybe:yes "certificatePolicies")
  (2 5 29 33)             -> (maybe:yes "policyMappings")
  (2 5 29 35)             -> (maybe:yes "authorityKeyIdentifier")
  (2 5 29 36)             -> (maybe:yes "policyConstraints")
  (2 5 29 37)             -> (maybe:yes "extKeyUsage")
  (2 5 29 46)             -> (maybe:yes "freshestCRL")
  (2 5 29 54)             -> (maybe:yes "inhibitAnyPolicy")
  (1 3 6 1 5 5 7 1 1)     -> (maybe:yes "authorityInfoAccess")
  (1 3 6 1 5 5 7 1 11)    -> (maybe:yes "subjectInfoAccess")
  (1 3 6 1 5 5 7 1 24)    -> (maybe:yes "tlsFeature")
  (1 3 6 1 4 1 11129 2 4 2) -> (maybe:yes "signedCertificateTimestamps")
  (1 3 6 1 4 1 11129 2 4 3) -> (maybe:yes "ctPrecertificatePoison")
  (1 3 6 1 4 1 311 20 2)    -> (maybe:yes "msCertificateTemplateName")
  (1 3 6 1 4 1 311 21 1)    -> (maybe:yes "msCAVersion")
  (1 3 6 1 4 1 311 21 7)    -> (maybe:yes "msCertificateTemplate")
  (1 3 6 1 4 1 311 21 10)   -> (maybe:yes "msApplicationPolicies")
  ;; extended key usages
  (2 5 29 37 0)           -> (maybe:yes "anyExtendedKeyUsage")
  (1 3 6 1 5 5 7 3 1)     -> (maybe:yes "serverAuth")
  (1 3 6 1 5 5 7 3 2)     -> (maybe:yes "clientAuth")
  (1 3 6 1 5 5 7 3 3)     -> (maybe:yes "codeSigning")
  (1 3 6 1 5 5 7 3 4)     -> (maybe:yes "emailProtection")
  (1 3 6 1 5 5 7 3 8)     -> (maybe:yes "timeStamping")
  (1 3 6 1 5 5 7 3 9)     -> (maybe:yes "OCSPSigning")
  ;; access methods
  (1 3 6 1 5 5 7 48 1)    -> (maybe:yes "OCSP")
  (1 3 6 1 5 5 7 48 2)    -> (maybe:yes "caIssuers")
  (1 3 6 1 5 5 7 48 5)    -> (maybe:yes "caRepository")
  ;; policies
  (2 5 29 32 0)           -> (maybe:yes "anyPolicy")
  (2 23 140 1 1)          -> (maybe:yes "CA/B extended-validation")
  (2 23 140 1 2 1)        -> (maybe:yes "CA/B domain-validated")
  (2 23 140 1 2 2)        -> (maybe:yes "CA/B organization-validated")
  (2 23 140 1 2 3)        -> (maybe:yes "CA/B individual-validated")
  (1 3 6 1 5 5 7 2 1)     -> (maybe:yes "CPS")
  (1 3 6 1 5 5 7 2 2)     -> (maybe:yes "userNotice")
  _ -> (maybe:no)
  )

;; "name" for a known OID, otherwise its dotted form.
(define (oid-label oid)
  (match (oid-name oid) with
    (maybe:yes name) -> name
    (maybe:no)       -> (oid-repr oid)
    ))

(define ber-oid-label
  (ber:OID oid) -> (oid-label oid)
  x             -> (ber-repr x)
  )

;; --------------------------------------------------------------------------------
;; formatting helpers
;; --------------------------------------------------------------------------------

;; "1a:2b:3c"
(define (hex-colon s)
  (let ((h (string->hex s))
        (parts (list:nil)))
    (for-range i (string-length s)
      (push! parts (substring h (* 2 i) (+ 2 (* 2 i)))))
    (string-join (reverse parts) ":")))

;; <s> as hex-colon lines of <n> bytes each.
(define (hex-lines s n)
  (let loop ((i 0) (acc (list:nil)))
    (if (>= i (string-length s))
        (reverse acc)
        (let ((j (min (string-length s) (+ i n))))
          (loop j (list:cons (hex-colon (substring s i j)) acc))))))

;; join <items> with ", ", breaking into lines no wider than <width>.
(define (wrap-commas items width)
  (let ((lines (list:nil))
        (line ""))
    (for-list item items
      (cond ((= 0 (string-length line))
             (set! line item))
            ((> (+ (string-length line) 2 (string-length item)) width)
             (push! lines (format line ","))
             (set! line item))
            (else
             (set! line (format line ", " item)))))
    (when (> (string-length line) 0)
      (push! lines line))
    (reverse lines)))

(define (concat-map f l)
  (let ((r (list:nil)))
    (for-list x l
      (for-list y (f x)
        (push! r y)))
    (reverse r)))

;; --------------------------------------------------------------------------------
;; names
;; --------------------------------------------------------------------------------

(define name-value-repr
  (ber:STRING (asn1string:BIT) s)   -> (format "#" (string->hex s))
  (ber:STRING (asn1string:OCTET) s) -> (format "#" (string->hex s))
  (ber:STRING _ s)                  -> s
  x                                 -> (ber-repr x)
  )

(define (dn-escape s)
  (let ((r (list:nil))
        (n (string-length s)))
    (for-range i n
      (let ((ch (string-ref s i)))
        (when (or (member? ch (string->list ",+\"\\<>;=") char=?)
                  (and (= i 0) (or (char=? ch #\#) (char=? ch #\space)))
                  (and (= i (- n 1)) (char=? ch #\space)))
          (push! r #\\))
        (push! r ch)))
    (list->string (reverse r))))

(define (attr-repr pair)
  (format (match (attr-name pair.oid) with
            (maybe:yes name) -> name
            (maybe:no)       -> (oid-repr pair.oid))
          "="
          (match pair.value with
            (ber:STRING (asn1string:BIT) _)   -> (name-value-repr pair.value)
            (ber:STRING (asn1string:OCTET) _) -> (name-value-repr pair.value)
            (ber:STRING _ s)                  -> (dn-escape s)
            x                                 -> (name-value-repr x))))

;; "C=US, O=Let's Encrypt, CN=YE1" (in encoded order, like openssl)
(define (x509-name-repr names)
  (format (join (lambda (rdn) (format (join attr-repr "+" rdn))) ", " names)))

;; just the last CN, or the whole name if there isn't one.
(define (x509-name-short names)
  (let ((cn (maybe:no)))
    (for-list rdn names
      (for-list pair rdn
        (when (oid=? pair.oid '(2 5 4 3))
          (set! cn (maybe:yes (name-value-repr pair.value))))))
    (match cn with
      (maybe:yes s) -> s
      (maybe:no)    -> (x509-name-repr names)
      )))

(define (x509-name=? a b)
  (string=? (x509-name-repr a) (x509-name-repr b)))

;; --------------------------------------------------------------------------------
;; time
;; --------------------------------------------------------------------------------

;; UTCTime YYMMDDHHMMSSZ or GeneralizedTime YYYYMMDDHHMMSSZ (rfc5280 4.1.2.5)
(define (x509-parse-time s)
  (define (d i)
    (let ((n (- (char->int (string-ref s i)) 48)))
      (if (and (>= n 0) (< n 10))
          n
          (raise (:X509/BadTime s)))))
  (define (d2 i) (+ (* 10 (d i)) (d (+ i 1))))
  (define (rest i year)
    {year=year month=(d2 i) day=(d2 (+ i 2))
     hour=(d2 (+ i 4)) minute=(d2 (+ i 6)) second=(d2 (+ i 8))})
  (match (string-length s) with
    13 -> (let ((yy (d2 0)))
            (rest 2 (if (< yy 50) (+ 2000 yy) (+ 1900 yy))))
    15 -> (rest 4 (+ (* 100 (d2 0)) (d2 2)))
    _  -> (raise (:X509/BadTime s))
    ))

(define (x509-time->unix s)
  (gregorian->unix (x509-parse-time s)))

(define (unix->date t)
  (let (((days secs) (divmod t 86400))
        (g (gregorian-from-absolute (+ days 719163))))
    {year=g.year month=g.month day=g.day
     hour=(/ secs 3600) minute=(mod (/ secs 60) 60) second=(mod secs 60)}))

(define (date-repr d)
  (format (int d.year) "-" (zpad 2 (int d.month)) "-" (zpad 2 (int d.day)) " "
          (zpad 2 (int d.hour)) ":" (zpad 2 (int d.minute)) ":" (zpad 2 (int d.second))
          " UTC"))

(define (x509-time-repr s)
  (date-repr (x509-parse-time s)))

;; --------------------------------------------------------------------------------
;; extensions
;; --------------------------------------------------------------------------------

(define (x509-find-extension cert oid)
  (let loop ((exts cert.exts))
    (match exts with
      () -> (maybe:no)
      (ext . rest) -> (if (oid=? ext.oid oid) (maybe:yes ext) (loop rest))
      )))

;; the subjectKeyIdentifier, if any.
(define (x509-subject-key-id cert)
  (match (x509-find-extension cert '(2 5 29 14)) with
    (maybe:yes ext)
    -> (match (ber->asn1 ext.value) with
         (ber:STRING (asn1string:OCTET) kid) -> (maybe:yes kid)
         _ -> (maybe:no))
    (maybe:no) -> (maybe:no)
    ))

;; the keyIdentifier from the authorityKeyIdentifier, if any.
(define (x509-authority-key-id cert)
  (match (x509-find-extension cert '(2 5 29 35)) with
    (maybe:yes ext)
    -> (match (ber->asn1 ext.value) with
         (ber:SEQUENCE ((ber:RAW (asn1class:ctxt) #f 0 kid) . _)) -> (maybe:yes kid)
         _ -> (maybe:no))
    (maybe:no) -> (maybe:no)
    ))

;; does basicConstraints say this is a CA?
(define (x509-ca? cert)
  (match (x509-find-extension cert '(2 5 29 19)) with
    (maybe:yes ext)
    -> (match (ber->asn1 ext.value) with
         (ber:SEQUENCE ((ber:BOOLEAN ca) . _)) -> ca
         _ -> #f)
    (maybe:no) -> #f
    ))

;; the names of the bits set in a keyUsage BIT STRING (rfc5280 4.2.1.3)
(define (key-usage-names bits)
  (let ((names #("digitalSignature" "nonRepudiation" "keyEncipherment"
                 "dataEncipherment" "keyAgreement" "keyCertSign" "cRLSign"
                 "encipherOnly" "decipherOnly"))
        (r (list:nil)))
    (for-range i 9
      (let ((byte (+ 1 (/ i 8))))
        (when (and (< byte (string-length bits))
                   (>0 (logand (>> #x80 (mod i 8)) (char->int (string-ref bits byte)))))
          (push! r names[i]))))
    (reverse r)))

;; does the cert allow signing certificates?  (no keyUsage means yes)
(define (x509-key-cert-sign? cert)
  (match (x509-find-extension cert '(2 5 29 15)) with
    (maybe:yes ext)
    -> (match (ber->asn1 ext.value) with
         (ber:STRING (asn1string:BIT) bits)
         -> (member? "keyCertSign" (key-usage-names bits) string=?)
         _ -> #f)
    (maybe:no) -> #t
    ))

(define (ipv4-repr s)
  (format (join (lambda (ch) (int->string (char->int ch))) "." (string->list s))))

(define (ipv6-repr s)
  ;; rfc5952: lowercase, no leading zeros, the longest run (>1) of
  ;;  zero groups replaced with "::".
  (let ((groups (list->vector
                 (map (lambda (i)
                        (+ (<< (char->int (string-ref s (* 2 i))) 8)
                           (char->int (string-ref s (+ 1 (* 2 i))))))
                      (range 8))))
        (best-start 0)
        (best-len 0))
    (define (group i)
      (if (= groups[i] 0) "0" (int->hex-string groups[i])))
    (define (join-groups start end)
      (let ((r (list:nil)))
        (for-range* i start end
          (push! r (group i)))
        (string-join (reverse r) ":")))
    (for-range i 8
      (let loop ((j i))
        (if (and (< j 8) (= groups[j] 0))
            (loop (+ j 1))
            (when (> (- j i) best-len)
              (set! best-start i)
              (set! best-len (- j i))))))
    (if (< best-len 2)
        (join-groups 0 8)
        (format (join-groups 0 best-start) "::" (join-groups (+ best-start best-len) 8)))))

(define (ip-repr s)
  (match (string-length s) with
    4  -> (ipv4-repr s)
    16 -> (ipv6-repr s)
    ;; address + mask, in nameConstraints
    8  -> (format (ipv4-repr (substring s 0 4)) "/" (ipv4-repr (substring s 4 8)))
    32 -> (format (ipv6-repr (substring s 0 16)) "/" (ipv6-repr (substring s 16 32)))
    _  -> (hex-colon s)
    ))

;; GeneralName (rfc5280 4.2.1.6)
(define general-name-repr
  (ber:RAW (asn1class:ctxt) #f 1 s) -> (format "email:" s)
  (ber:RAW (asn1class:ctxt) #f 2 s) -> (format "DNS:" s)
  (ber:RAW (asn1class:ctxt) #f 6 s) -> (format "URI:" s)
  (ber:RAW (asn1class:ctxt) #f 7 s) -> (format "IP:" (ip-repr s))
  (ber:RAW (asn1class:ctxt) #f 8 s) -> (format "RID:" (hex-colon s))
  (ber:CONTEXT 0 ((ber:OID oid) . _)) -> (format "otherName:" (oid-label oid))
  (ber:CONTEXT 4 (name))            -> (format "DirName:" (x509-name-repr (x509-parse-name name)))
  x                                 -> (ber-repr x)
  )

(define basic-constraints-repr
  ()                                 -> "CA:FALSE"
  ((ber:BOOLEAN ca))                 -> (format "CA:" (if ca "TRUE" "FALSE"))
  ((ber:BOOLEAN ca) (ber:INTEGER n)) -> (format "CA:" (if ca "TRUE" "FALSE") ", pathlen:" (int n))
  ((ber:INTEGER n))                  -> (format "CA:FALSE, pathlen:" (int n))
  x                                  -> (ber-repr (ber:SEQUENCE x))
  )

(define aki-item-repr
  (ber:RAW (asn1class:ctxt) #f 0 kid)    -> (format "keyid:" (hex-colon kid))
  (ber:CONTEXT 1 names)                  -> (format "issuer:" (join general-name-repr ", " names))
  (ber:RAW (asn1class:ctxt) #f 2 serial) -> (format "serial:" (hex-colon serial))
  x                                      -> (ber-repr x)
  )

;; AccessDescription ::= SEQUENCE { accessMethod OID, accessLocation GeneralName }
(define access-desc-repr
  (ber:SEQUENCE ((ber:OID method) name)) -> (format (oid-label method) " - " (general-name-repr name))
  x                                      -> (ber-repr x)
  )

;; PolicyInformation ::= SEQUENCE { policyIdentifier OID, policyQualifiers SEQUENCE OF ... OPTIONAL }
(define policy-lines
  (ber:SEQUENCE ((ber:OID oid)))
  -> (list (oid-label oid))
  (ber:SEQUENCE ((ber:OID oid) (ber:SEQUENCE quals)))
  -> (list:cons (oid-label oid) (map policy-qualifier-repr quals))
  x -> (list (ber-repr x))
  )

(define policy-qualifier-repr
  (ber:SEQUENCE ((ber:OID (1 3 6 1 5 5 7 2 1)) (ber:STRING _ uri)))
  -> (format "  CPS: " uri)
  (ber:SEQUENCE ((ber:OID (1 3 6 1 5 5 7 2 2)) . _))
  -> "  userNotice"
  x -> (format "  " (ber-repr x))
  )

;; DistributionPoint ::= SEQUENCE {
;;   distributionPoint [0] DistributionPointName OPTIONAL,
;;   reasons           [1] ReasonFlags OPTIONAL,
;;   cRLIssuer         [2] GeneralNames OPTIONAL }
;; DistributionPointName ::= CHOICE {
;;   fullName                [0] GeneralNames,
;;   nameRelativeToCRLIssuer [1] RelativeDistinguishedName }
(define distribution-point-lines
  (ber:SEQUENCE parts) -> (concat-map dp-part-lines parts)
  x                    -> (list (ber-repr x))
  )

(define dp-part-lines
  (ber:CONTEXT 0 ((ber:CONTEXT 0 names)))  -> (map general-name-repr names)
  (ber:RAW (asn1class:ctxt) #f 1 bits)     -> (list (format "reasons:" (hex-colon bits)))
  (ber:CONTEXT 2 names)                    -> (list (format "cRLIssuer:" (join general-name-repr ", " names)))
  x                                        -> (list (ber-repr x))
  )

(define name-constraint-lines
  (ber:CONTEXT 0 subtrees) -> (list:cons "permitted:" (map subtree-repr subtrees))
  (ber:CONTEXT 1 subtrees) -> (list:cons "excluded:" (map subtree-repr subtrees))
  x                        -> (list (ber-repr x))
  )

(define subtree-repr
  (ber:SEQUENCE (name . _)) -> (format "  " (general-name-repr name))
  x                         -> (format "  " (ber-repr x))
  )

(define tls-feature-repr
  (ber:INTEGER 5)  -> "status_request (OCSP must-staple)"
  (ber:INTEGER 17) -> "status_request_v2"
  x                -> (ber-repr x)
  )

;; SignedCertificateTimestampList (rfc6962 3.3), a TLS-encoded list
;;  inside the OCTET STRING:
;;   opaque SerializedSCT<1..2^16-1>;
;;   struct { SerializedSCT sct_list <1..2^16-1>; } SignedCertificateTimestampList;
;;  where each SCT is:
;;   version(1) log_id(32) timestamp(8) extensions<0..2^16-1>
;;   hash_algorithm(1) signature_algorithm(1) signature<0..2^16-1>
(define (sct-lines s)
  (define (u8 i) (char->int (string-ref s i)))
  (define (u16 i) (+ (<< (u8 i) 8) (u8 (+ i 1))))
  (define (u64 i)
    (let loop ((j 0) (n 0))
      (if (= j 8) n (loop (+ j 1) (+ (<< n 8) (u8 (+ i j)))))))
  (define hash-name 2 -> "sha1" 4 -> "sha256" 5 -> "sha384" 6 -> "sha512" n -> (int->string n))
  (define sig-name  1 -> "rsa" 3 -> "ecdsa" 7 -> "ed25519" n -> (int->string n))
  (let ((len (string-length s))
        (r (list:nil)))
    (if (or (< len 2) (not (= len (+ 2 (u16 0)))))
        (list (format "(bad SCT list) " (string->hex s)))
        (let loop ((pos 2))
          (if (>= pos len)
              (reverse r)
              (let ((sctlen (u16 pos))
                    (p (+ pos 2)))
                (if (or (> (+ p sctlen) len) (< sctlen 45))
                    (reverse (list:cons "(bad SCT)" r))
                    (let ((version (u8 p))
                          (logid (substring s (+ p 1) (+ p 33)))
                          (ts (u64 (+ p 33)))
                          (q (+ p 43 (u16 (+ p 41)))))
                      (push! r (format (date-repr (unix->date (/ ts 1000)))
                                       "  v" (int (+ version 1))
                                       " " (hash-name (u8 q)) "/" (sig-name (u8 (+ q 1)))
                                       "  log " (b64-encode logid)))
                      (loop (+ p sctlen))))))))))

;; a human-readable rendering of an extension's value, as lines.
(define (extension-lines ext)
  (match ext.oid (ber->asn1 ext.value) with
    (2 5 29 15) (ber:STRING (asn1string:BIT) bits)
    -> (list (string-join (key-usage-names bits) ", "))
    (2 5 29 37) (ber:SEQUENCE oids)
    -> (wrap-commas (map ber-oid-label oids) 70)
    (2 5 29 19) (ber:SEQUENCE items)
    -> (list (basic-constraints-repr items))
    (2 5 29 14) (ber:STRING (asn1string:OCTET) kid)
    -> (list (hex-colon kid))
    (2 5 29 35) (ber:SEQUENCE items)
    -> (map aki-item-repr items)
    (2 5 29 17) (ber:SEQUENCE names)
    -> (wrap-commas (map general-name-repr names) 70)
    (2 5 29 18) (ber:SEQUENCE names)
    -> (wrap-commas (map general-name-repr names) 70)
    (1 3 6 1 5 5 7 1 1) (ber:SEQUENCE descs)
    -> (map access-desc-repr descs)
    (1 3 6 1 5 5 7 1 11) (ber:SEQUENCE descs)
    -> (map access-desc-repr descs)
    (2 5 29 32) (ber:SEQUENCE policies)
    -> (concat-map policy-lines policies)
    (2 5 29 31) (ber:SEQUENCE points)
    -> (concat-map distribution-point-lines points)
    (2 5 29 46) (ber:SEQUENCE points)
    -> (concat-map distribution-point-lines points)
    (2 5 29 30) (ber:SEQUENCE parts)
    -> (concat-map name-constraint-lines parts)
    (2 5 29 54) (ber:INTEGER n)
    -> (list (format "skipCerts:" (int n)))
    (1 3 6 1 5 5 7 1 24) (ber:SEQUENCE features)
    -> (map tls-feature-repr features)
    (1 3 6 1 4 1 11129 2 4 2) (ber:STRING (asn1string:OCTET) scts)
    -> (sct-lines scts)
    (1 3 6 1 4 1 11129 2 4 3) (ber:NULL)
    -> (list "(this is a precertificate)")
    _ x
    -> (list (ber-repr x))
    ))

;; --------------------------------------------------------------------------------
;; signatures
;; --------------------------------------------------------------------------------

(datatype sigcheck
  (:good)
  (:bad)
  (:unsupported string)
  )

(define public-key-kind
  (public-key:rsa n _)  -> (format "RSA-" (int (big->bits n)))
  (public-key:ed25519 _) -> "Ed25519"
  (public-key:ed448 _)   -> "Ed448"
  (public-key:ec _ oid)  -> (format "EC " (oid-label oid))
  )

;; check the signature on <x509> using <key>, the public key of its
;;  (purported) issuer.
(define (x509-check-signature x509 key)
  (define (result ok?)
    (if ok? (sigcheck:good) (sigcheck:bad)))
  (define (ecdsa hash curve k)
    (match (ec-curve-by-oid curve) with
      (maybe:yes c) -> (result (ecdsa-verify c (hash x509.tbs) x509.sig k))
      (maybe:no)    -> (sigcheck:unsupported (format "ECDSA on " (oid-label curve)))
      ))
  (define (rsa n e hash hash-oid)
    (result (try (rsa-verify-hash n (int->big e) x509.tbs x509.sig hash hash-oid)
                 except (:SIG/SigWrongSize _ _) -> #f)))
  (match x509.algo.oid key with
    (1 2 840 10045 4 3 2) (public-key:ec k curve) -> (ecdsa sha256 curve k)
    (1 2 840 10045 4 3 3) (public-key:ec k curve) -> (ecdsa sha384 curve k)
    (1 2 840 10045 4 3 4) (public-key:ec k curve) -> (ecdsa sha512 curve k)
    (1 3 101 112) (public-key:ed25519 pk)         -> (result (ed25519-verify x509.sig x509.tbs pk))
    (1 2 840 113549 1 1 11) (public-key:rsa n e) -> (rsa n e sha256 '(2 16 840 1 101 3 4 2 1))
    (1 2 840 113549 1 1 12) (public-key:rsa n e) -> (rsa n e sha384 '(2 16 840 1 101 3 4 2 2))
    (1 2 840 113549 1 1 13) (public-key:rsa n e) -> (rsa n e sha512 '(2 16 840 1 101 3 4 2 3))
    oid key
    -> (sigcheck:unsupported (format (oid-label oid) " with an " (public-key-kind key) " key"))
    ))

;; --------------------------------------------------------------------------------
;; printing
;; --------------------------------------------------------------------------------

(define (public-key-lines key verbose?)
  (match key with
    (public-key:rsa n e)
    -> (list:cons (format "RSA " (int (big->bits n)) " bits, e=" (int e))
                  (if verbose? (hex-lines (big->u256 n) 16) (list:nil)))
    (public-key:ec k oid)
    -> (list:cons (format "EC " (oid-label oid))
                  (if verbose? (hex-lines k 16) (list:nil)))
    (public-key:ed25519 k) -> (list "Ed25519" (hex-colon k))
    (public-key:ed448 k)   -> (list:cons "Ed448" (hex-lines k 16))
    ))

;; how a validity period compares with <now> (unix time)
(define (validity-status validity now)
  (let ((nb (x509-time->unix validity.not-before))
        (na (x509-time->unix validity.not-after)))
    (cond ((< now nb) (format (ansi (bold red) "not yet valid")))
          ((> now na) (format (ansi (bold red) "EXPIRED") " " (int (/ (- now na) 86400)) " days ago"))
          (else (format (ansi green "valid") ", " (int (/ (- na now) 86400)) " days left")))))

(define (print-labeled label lines)
  (match lines with
    () -> (printf (bold (rpad 12 label)) "\n")
    (line . rest)
    -> (begin
         (printf (bold (rpad 12 label)) line "\n")
         (for-list line rest
           (printf (repeat 12 " ") line "\n")))
    ))

(define (print-extension ext)
  (printf "  " (bold (oid-label ext.oid))
          (if ext.critical (format " " (ansi yellow "[critical]")) "")
          "\n")
  (for-list line (extension-lines ext)
    (printf "      " line "\n")))

;; <now> is the current unix time, for checking validity.
(define (print-x509 x now verbose?)
  (let ((cert x.cert))
    (print-labeled "subject" (list (x509-name-repr cert.subject)))
    (print-labeled "issuer" (list (x509-name-repr cert.issuer)))
    (print-labeled "serial" (list (hex-colon (big->u256 cert.serial))))
    (when (not (= cert.version 3))
      (print-labeled "version" (list (int->string cert.version))))
    (print-labeled "not before" (list (x509-time-repr cert.validity.not-before)))
    (print-labeled "not after" (list (format (x509-time-repr cert.validity.not-after)
                                             "  (" (validity-status cert.validity now) ")")))
    (print-labeled "key" (public-key-lines cert.pubkey.key verbose?))
    (print-labeled "signature" (list:cons (format (oid-label x.algo.oid)
                                                  " (" (int (string-length x.sig)) " bytes)")
                                          (if verbose? (hex-lines x.sig 16) (list:nil))))
    (when (not (oid=? cert.sig-kind.oid x.algo.oid))
      (print-labeled "" (list (format (ansi (bold red) "tbsCertificate says ")
                                      (oid-label cert.sig-kind.oid)))))
    (print-labeled "sha256" (hex-lines (sha256 x.der) 16))
    (when (not (null? cert.exts))
      (printf (bold "extensions") "\n")
      (for-list ext cert.exts
        (print-extension ext)))
    ))

;; check the signature on a self-signed cert.
(define (verify-cert x509)
  (let ((der x509.tbs))
    (match x509.cert.pubkey.key with
      (public-key:rsa n e)
      -> (printf "verify: " (bool (rsa-verify n (int->big e) der x509.sig)) "\n")
      (public-key:ed25519 pk)
      -> (printf "verify: " (bool (ed25519-verify x509.sig der pk)) "\n")
      _ -> (printf "verify: -- key type not supported --\n")
      )))
