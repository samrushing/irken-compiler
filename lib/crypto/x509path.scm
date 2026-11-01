;; -*- Mode: Irken -*-

;; certificate path validation for a TLS client: is this chain, as
;;  presented by a server, good for this host name?
;;
;; this is a simple subset of rfc5280 section 6: we build a path from
;;  the leaf through the presented certificates to a trust anchor,
;;  checking names, signatures, validity periods, basicConstraints and
;;  keyUsage.  not (yet) checked: name constraints, policies,
;;  pathLenConstraint, and revocation.

(require "lib/crypto/x509.scm")

;; parse what we can of a bundle of trust anchors (e.g. /etc/ssl/cert.pem),
;;  quietly skipping any certificate we can't parse.
(define (x509-load-anchors path)
  (let ((r (list:nil)))
    (for-list der (read-certificates path)
      (try
       (push! r (parse-x509 der))
       except
       (:X509/Unexpected _ _)  -> #u
       (:X509/Unsupported _ _) -> #u
       (:ASN1/ExtraData _ _)   -> #u
       (:BER/ExtraData _ _)    -> #u
       ))
    (reverse r)))

;; the usual places for the system's bundle of trusted roots.
(define x509-system-anchor-paths
  '("/etc/ssl/cert.pem"                                   ;; macOS, FreeBSD, OpenBSD
    "/usr/local/share/certs/ca-root-nss.crt"              ;; FreeBSD (ca_root_nss)
    "/etc/ssl/certs/ca-certificates.crt"                  ;; Debian, Ubuntu, Alpine
    "/etc/pki/tls/certs/ca-bundle.crt"                    ;; Fedora, RHEL
    ))

;; --- host names (rfc6125) ---

(define (ipv4-literal? s)
  (let ((parts (string-split s #\.)))
    (and (= 4 (length parts))
         (every? (lambda (p)
                   (and (> (string-length p) 0)
                        (<= (string-length p) 3)
                        (every? digit? (string->list p))
                        (< (string->int p) 256)))
                 parts))))

;; the dNSNames and iPAddresses in the subjectAltName.
(define (x509-alt-names cert)
  (let ((dns (list:nil))
        (ips (list:nil)))
    (match (x509-find-extension cert '(2 5 29 17)) with
      (maybe:yes ext)
      -> (match (ber->asn1 ext.value) with
           (ber:SEQUENCE names)
           -> (for-list name names
                (match name with
                  (ber:RAW (asn1class:ctxt) #f 2 s) -> (push! dns (downcase s))
                  (ber:RAW (asn1class:ctxt) #f 7 s) -> (push! ips (ip-repr s))
                  _ -> #u))
           _ -> #u)
      (maybe:no) -> #u)
    {dns=(reverse dns) ips=(reverse ips)}))

;; "*.example.com" matches exactly one (whole) leftmost label.
(define (host-matches? pattern host)
  (if (and (> (string-length pattern) 2)
           (string=? "*." (substring pattern 0 2)))
      (let ((i (string-find "." host)))
        (and (> i 0)
             (string=? (substring pattern 1 (string-length pattern))
                       (substring host i (string-length host)))))
      (string=? pattern host)))

(define (x509-valid-for-host? cert host)
  (let ((names (x509-alt-names cert))
        (host (downcase host)))
    (if (ipv4-literal? host)
        (member? host names.ips string=?)
        (some? (lambda (pattern) (host-matches? pattern host)) names.dns))))

;; --- paths ---

(define (x509-describe x)
  (format "'" (x509-name-short x.cert.subject) "'"))

;; check that <x> is within its validity period.
(define (x509-check-validity x now)
  (let ((v x.cert.validity))
    (when (< now (x509-time->unix v.not-before))
      (raise (:X509/Invalid (format "certificate " (x509-describe x) " is not valid until "
                                    (x509-time-repr v.not-before)))))
    (when (> now (x509-time->unix v.not-after))
      (raise (:X509/Invalid (format "certificate " (x509-describe x) " expired "
                                    (x509-time-repr v.not-after)))))))

;; does the extendedKeyUsage (if any) allow TLS server authentication?
(define (x509-server-auth? x)
  (match (x509-find-extension x.cert '(2 5 29 37)) with
    (maybe:yes ext)
    -> (match (ber->asn1 ext.value) with
         (ber:SEQUENCE oids)
         -> (some? (lambda (o)
                     (match o with
                       (ber:OID oid) -> (or (oid=? oid '(1 3 6 1 5 5 7 3 1))
                                            (oid=? oid '(2 5 29 37 0)))
                       _ -> #f))
                   oids)
         _ -> #f)
    (maybe:no) -> #t
    ))

;; did <issuer> sign <x>?  raises if the signature is bad or can't be checked.
(define (x509-check-issued-by x issuer)
  (match (x509-check-signature x issuer.cert.pubkey.key) with
    (sigcheck:good) -> #u
    (sigcheck:bad)
    -> (raise (:X509/Invalid (format "bad signature on " (x509-describe x)
                                     " by " (x509-describe issuer))))
    (sigcheck:unsupported why)
    -> (raise (:X509/Invalid (format "can't check the signature on " (x509-describe x)
                                     ": " why)))
    ))

;; validate the chain <ders> (DER, leaf first, as presented by a
;;  server) for <host>, at unix time <now>, against the trust anchors
;;  <anchors> (parsed x509s).  an anchor may also be the leaf itself,
;;  i.e. a pinned certificate.  returns the parsed leaf, or raises
;;  (:X509/Invalid reason).
(define (x509-verify-server ders anchors host now)
  (let ((chain (map (lambda (der)
                      (try
                       (parse-x509 der)
                       except
                       (:X509/Unexpected what _)
                       -> (raise (:X509/Invalid (format "can't parse certificate (" what ")")))
                       (:X509/Unsupported what _)
                       -> (raise (:X509/Invalid (format "unsupported certificate (" what ")")))))
                    ders)))
    (when (null? chain)
      (raise (:X509/Invalid "no certificates")))
    (let ((leaf (first chain)))

      (define (anchor? x)
        (some? (lambda (a) (string=? a.der x.der)) anchors))

      ;; anchors and presented certs that could have issued <x>
      (define (candidates x pool)
        (filter (lambda (c)
                  (and (not (string=? c.der x.der))
                       (x509-name=? x.cert.issuer c.cert.subject)))
                pool))

      (define (ca-ok? c)
        (and (x509-ca? c.cert) (x509-key-cert-sign? c.cert)))

      ;; walk up from <x>; <depth> limits the path length.
      (define (walk x depth)
        (x509-check-validity x now)
        (cond ((anchor? x) #u) ;; trusted directly.
              ((> depth 8) (raise (:X509/Invalid "certificate chain too long")))
              (else
               (match (candidates x anchors) (candidates x chain) with
                 (a . _) _
                 -> (begin
                      (when (not (ca-ok? a))
                        (raise (:X509/Invalid (format "trust anchor " (x509-describe a)
                                                      " is not a CA"))))
                      (x509-check-issued-by x a)
                      (x509-check-validity a now))
                 () (c . _)
                 -> (begin
                      (when (not (ca-ok? c))
                        (raise (:X509/Invalid (format (x509-describe c) " is not a CA, but issued "
                                                      (x509-describe x)))))
                      (x509-check-issued-by x c)
                      (walk c (+ depth 1)))
                 () ()
                 -> (raise (:X509/Invalid
                            (format "no trusted root for " (x509-describe x)
                                    " (issuer " (x509-name-repr x.cert.issuer) ")")))
                 ))))

      (when (not (x509-valid-for-host? leaf.cert host))
        (let ((names (x509-alt-names leaf.cert)))
          (raise (:X509/Invalid
                  (format "certificate " (x509-describe leaf) " is not valid for " host
                          " (it names " (join ", " (append names.dns names.ips)) ")")))))
      (when (not (x509-server-auth? leaf))
        (raise (:X509/Invalid (format "certificate " (x509-describe leaf)
                                      " is not for TLS servers (extKeyUsage)"))))
      (walk leaf 0)
      leaf)))
