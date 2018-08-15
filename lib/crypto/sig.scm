;; -*- Mode: Irken -*-

;; PKCS #1 v2.2 (RFC 8017)

(define (read-pem-asn1 path)
  (with-file file (file/open-read path)
    (match ((pem-gen (file-char-generator file))) with
      (maybe:yes (:tuple kind ber))
      -> (:tuple kind (ber->asn1 ber))
      _
      -> (raise (:SIG/NoPEMFound path))
      )))

(define (read-skey path)
  (match (read-pem-asn1 path) with
    (:tuple "RSA PRIVATE KEY" asn1) -> (skey:rsa (decode-rsa-skey asn1))
    (:tuple "PRIVATE KEY" asn1)     -> (decode-skey asn1)
    (:tuple kind _)                 -> (raise (:SIG/UnexpectedPEM kind))
    ))

(datatype skey
  (:rsa {n=big e=int d=big p=big q=big dP=big dQ=big qInv=big})
  (:ed25519 string)
  (:ed448 string)
  ;; ECDSA
  )

(define decode-pkcs8
  (ber:SEQUENCE
   ((ber:INTEGER 0) (ber:SEQUENCE ((ber:OID oid) (ber:NULL)))
    (ber:STRING (asn1string:OCTET) encapsulated)))
  -> (:tuple oid encapsulated)
  (ber:SEQUENCE
   ((ber:INTEGER 0) (ber:SEQUENCE ((ber:OID oid))) ;; note: no NULL
    (ber:STRING (asn1string:OCTET) encapsulated)))
  -> (:tuple oid encapsulated)
  x -> (raise (:SIG/ExpectedPKCS8 x))
  )

;; https://tools.ietf.org/html/draft-ietf-curdle-pkix-10
;; XXX need to handle attributes & public key...
(define decode-eddsa-skey
  (ber:STRING (asn1string:OCTET) key) -> key
  x -> (raise (:SIG/UnexpectedASN1 x))
  )

(define (decode-skey asn1)
  (let (((oid encapsulated) (decode-pkcs8 asn1)))
    (match oid with
      (1 2 840 113549 1 1 1) -> (skey:rsa (decode-rsa-skey (ber->asn1 encapsulated)))
      (1 3 101 112)          -> (skey:ed25519 (decode-eddsa-skey (ber->asn1 encapsulated)))
      (1 3 101 113)          -> (skey:ed448 (decode-eddsa-skey (ber->asn1 encapsulated)))
      _                      -> (raise (:SIG/UnsupportedPrivateKey oid))
      )))

(define decode-rsa-skey
  (ber:SEQUENCE
   ((ber:INTEGER 0)        ;; version
    (ber:BINTEGER modulus) ;; n
    (ber:INTEGER  pub-exp) ;; e
    (ber:BINTEGER pri-exp) ;; d
    (ber:BINTEGER prime1)  ;; p
    (ber:BINTEGER prime2)  ;; q
    (ber:BINTEGER exp1)    ;; d mod (p-1)
    (ber:BINTEGER exp2)    ;; d mod (q-1)
    (ber:BINTEGER qInv)    ;; qInv mod p
    . other-prime-info     ;; optional (for multi-prime RSA)
    )) -> {n    = (b256->big modulus)
           e    = pub-exp
           d    = (b256->big pri-exp)
           p    = (b256->big prime1)
           q    = (b256->big prime2)
           dP   = (b256->big exp1)
           dQ   = (b256->big exp2)
           qInv = (b256->big qInv)
           }
   val -> (raise (:SIG/InvalidKey (ber-repr val)))
   )

;; assumes sha256
;; 9.2 EMSA-PKCS1-v1_5
(define (pad-and-annotate digest mlen)
  (let ((diglen (string-length digest))
        (asn1 (ber:SEQUENCE
               (LIST
                (ber:SEQUENCE
                 (LIST (ber:OID '(2 16 840 1 101 3 4 2 1))
                       (ber:NULL)))
                (ber:STRING (asn1string:OCTET) digest))))
        (T (asn1->ber asn1))
        (tlen (string-length T)))
    (when (not (= diglen 32))
      (raise (:SIG/DigestWrongSize digest)))
    (when (< mlen (+ 11 tlen))
      (raise (:SIG/MlenTooShort mlen)))
    (let ((PS (format (repeat (- mlen tlen 3) "\xff"))))
      (string-append "\x00\x01" PS "\x00" T))))

;; 4.1 integer to octet string
(define (I2OSP n len)
  (let ((r (big->u256 n))
        (rlen (string-length r)))
    (match (int-cmp rlen len) with
      (cmp:<) -> (format (repeat (- len rlen) "\x00") r)
      (cmp:=) -> r
      (cmp:>) -> (raise (:SIG/IntegerTooLarge n))
      )))

;; 4.2 octet-string to integer
(define (OS2IP s)
  (u256->big s))

(define (big/nbytes n)
  (let (((q r) (divmod (big->bits n) 8)))
    (if (= r 0) q (+ q 1))))

;; 5.2.1 RSASP1

;; (define (RSASP1 skey m)
;;   (let ((m1 (big-exp-mod m skey.dP skey.p))
;;         (m2 (big-exp-mod m skey.dQ skey.q))
;;         (h (big-mod (big-mul skey.qInv (big-sub m1 m2)) skey.p)))
;;     (big-add m2 (big-mul skey.q h))))

;; This version uses the translated version of BearSSL's constant-time i31 code.

(define (RSASP1 skey m)
  (i31/rsa-crt m skey))

;; 8.2.1
(define (rsa-sign skey msg)
  (let ((H (sha256 msg))
        (nbytes (big/nbytes skey.n))
        (padded (pad-and-annotate H nbytes))
        (s (big-exp-mod (b256->big padded) skey.d skey.n)))
    ;; (printf " --- sign ---\n")
    ;; (printf "digest " (string->hex H) "\n")
    ;; (printf "nbytes " (int nbytes) "\n")
    (I2OSP s nbytes)
    ))

;; 8.2.2
(define (rsa-verify n e M S)
  (let ((nbytes (big/nbytes n))
        (Slen (string-length S))
        (s (OS2IP S))
        (m (big-exp-mod s e n))
        (EM0 (I2OSP m nbytes))
        (EM1 (pad-and-annotate (sha256 M) nbytes)))
    ;; (printf " --- verify ---\n")
    ;; (printf "s " (big-repr s) "\n")
    ;; (printf "H   " (string->hex (sha256 M)) "\n")
    ;; (printf "EM0 " (string->hex EM0) "\n")
    ;; (printf "EM1 " (string->hex EM1) "\n")
    (when (not (= nbytes Slen))
      (raise (:SIG/SigWrongSize nbytes Slen)))
    (string=? EM0 EM1)))

(define (xor-strings a b)
  (when (not (= (string-length a) (string-length b)))
    (raise (:SIG/XorStringUnequalLength a b)))
  (let ((result (make-string (string-length a))))
    (for-range i (string-length a)
      (string-set!
       result i
       (int->char
        (logxor (char->int (string-ref a i))
                (char->int (string-ref b i))))))
    result))

;; 9.1.1
(define (emsa-pss RNG hash sLen)

  (define (MGF1 seed maskLen)
    (let ((T ""))
      (for-range i (how-many maskLen hash.size)
        (set! T (string-append
                 T
                 (hash.oneshot
                  (string-append
                   seed
                   (I2OSP (int->big i) 4))))))
      (substring T 0 maskLen)
      ))

  (define (encode M emBits)
    (let ((emLen (how-many emBits 8))
          (emExtra (- (* emLen 8) emBits))
          (eight-zeros (format (repeat 8 "\x00")))
          (PS (format (repeat (- emLen sLen hash.size 2) "\x00")))
          (mHash (hash.oneshot M))
          (salt (RNG sLen))
          (M0 (string-append eight-zeros mHash salt))
          (H (hash.oneshot M0))
          (DB (string-append PS "\x01" salt))
          (dbMask (MGF1 H (- emLen hash.size 1)))
          (maskedDB (xor-strings DB dbMask))
          )
      ;; XXX this probably needs to go _before_ we do all the work.
      (when (< emLen (+ hash.size sLen 2))
        (raise (:SIG/EncodingError)))
      ;; step 11: set some of the leftmost bits of maskedDB to zero.
      ;; [or, can we just enforce bits are multiples of 8?]
      ;; clear the leftmost emExtra bits.
      (string-set! maskedDB 0
                   (int->char (logand (>> #xff emExtra)
                                      (char->int (string-ref maskedDB 0)))))
      (string-append maskedDB H "\xbc")
      ))
  ;; body of esma-pss
  ;; interface
  {encode=encode}
  )

;; 8.1.1
(define (rsassa-pss-sign skey msg hash RNG)
  (let ((PSS (emsa-pss RNG hash hash.size))
        (modbits (big->bits skey.n))
        (EM (PSS.encode msg (- modbits 1)))
        (m (OS2IP EM))
        ;;(s (big-exp-mod m skey.d skey.n))
        (s (RSASP1 skey m))
        (nbytes (big/nbytes skey.n)))
    (I2OSP s nbytes)
    ))

