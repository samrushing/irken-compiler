;; -*- Mode: Irken -*-

;; describe X.509 certificates, and check a chain of them.
;;
;; $ demo/crypto/x509tool chain.pem
;;
;; prints each certificate (names, validity, key, decoded extensions),
;;  then, for each one, looks for its issuer among the input
;;  certificates and checks the link: issuer name, key identifiers,
;;  basicConstraints/keyUsage of the issuer, and the signature.
;;
;; signatures that can be checked: ECDSA on P-256/P-384 (with SHA-256,
;;  -384 or -512), Ed25519, and sha256WithRSAEncryption.

(require "lib/basis.scm")
(require "lib/getopt.scm")
(require "lib/time.scm")
(require "lib/asn1/ber.scm")
(require "lib/crypto/x509.scm")

(define program-options
  (makeopt
   (flag 'v)
   (flag 'a)
   (flag 'h)
   (pos+ 'files (string ""))
   ))

(define (usage)
  (printf "describe X.509 certificates, and check the chain they form.\n\n"
          "Usage: " sys.argv[0] " [-v] [-a] file ...\n"
          "  file   PEM (any number of certificates) or DER\n"
          "  [-v]   also print keys and signatures in hex\n"
          "  [-a]   also dump each certificate's ASN.1\n"
          "  [-h]   print this usage.\n")
  (%exit #f -1))

(define (status-repr s)
  (match s with
    (sigcheck:good)          -> (format (ansi green "good"))
    (sigcheck:bad)           -> (format (ansi (bold red) "BAD"))
    (sigcheck:unsupported s) -> (format (ansi yellow "not checked") " (" s ")")
    ))

;; the index of the issuer of certs[i]: itself if self-issued, else
;;  preferably the next one, else any other with a matching subject.
(define (find-issuer certs i)
  (let ((x certs[i])
        (n (vector-length certs)))
    (define (issued-by? j)
      (let ((y certs[j]))
        (x509-name=? x.cert.issuer y.cert.subject)))
    (cond ((issued-by? i) (maybe:yes i))
          ((and (< (+ i 1) n) (issued-by? (+ i 1))) (maybe:yes (+ i 1)))
          (else
           (let loop ((j 0))
             (cond ((= j n) (maybe:no))
                   ((and (not (= j i)) (issued-by? j)) (maybe:yes j))
                   (else (loop (+ j 1)))))))))

;; check the link from certs[i] to its issuer certs[j].
;;  returns the number of problems found.
(define (check-link certs i j)
  (let ((x certs[i])
        (issuer certs[j])
        (problems 0))
    (define (problem msg)
      (inc! problems 1)
      (printf "      " (ansi (bold red) "problem: ") msg "\n"))
    (printf "      issued by " (if (= i j) "itself" (format "[" (int j) "]")) "\n")
    (match (x509-authority-key-id x.cert) (x509-subject-key-id issuer.cert) with
      (maybe:yes aki) (maybe:yes ski)
      -> (if (string=? aki ski)
             (printf "      key id    matches\n")
             (problem (format "authorityKeyIdentifier " (hex-colon aki)
                              " is not the issuer's key id " (hex-colon ski))))
      _ _ -> #u)
    (when (not (x509-ca? issuer.cert))
      (if (= i j)
          (printf "      (self-signed, not a CA)\n")
          (problem "the issuer is not a CA (basicConstraints)")))
    (when (not (x509-key-cert-sign? issuer.cert))
      (problem "the issuer's keyUsage does not include keyCertSign"))
    (let ((check (x509-check-signature x issuer.cert.pubkey.key)))
      (printf "      signature " (status-repr check) "\n")
      (match check with
        (sigcheck:bad) -> (inc! problems 1)
        _ -> #u))
    problems))

;; returns the number of problems found.
(define (check-chain certs now)
  (let ((n (vector-length certs))
        (problems 0)
        (unchecked 0))
    (printf "\n" (bold "chain") "\n")
    (for-range i n
      (let ((x certs[i]))
        (printf "  [" (int i) "] " (x509-name-short x.cert.subject) "\n")
        (when (< now (x509-time->unix x.cert.validity.not-before))
          (inc! problems 1)
          (printf "      " (ansi (bold red) "problem: ") "not valid until "
                  (x509-time-repr x.cert.validity.not-before) "\n"))
        (when (> now (x509-time->unix x.cert.validity.not-after))
          (inc! problems 1)
          (printf "      " (ansi (bold red) "problem: ") "expired "
                  (x509-time-repr x.cert.validity.not-after) "\n"))
        (match (find-issuer certs i) with
          (maybe:yes j)
          -> (begin
               (when (and (not (= j i)) (not (= j (+ i 1))))
                 (printf "      (out of order)\n"))
               (inc! problems (check-link certs i j))
               (match (let ((issuer certs[j]))
                        (x509-check-signature x issuer.cert.pubkey.key)) with
                 (sigcheck:unsupported _) -> (inc! unchecked 1)
                 _ -> #u))
          (maybe:no)
          -> (printf "      issued by " (x509-name-repr x.cert.issuer) "\n"
                     "      (not in the input)\n"))))
    (printf "\n"
            (if (= problems 0)
                (format (ansi green "no problems found"))
                (format (ansi (bold red) (int problems) " problem" (if (= problems 1) "" "s"))))
            (if (> unchecked 0)
                (format ", " (int unchecked) " signature" (if (= unchecked 1) "" "s") " not checked")
                "")
            "\n")
    problems))

(define (main)
  (set-verbose-gc #f)
  (let ((opts (try
               (process-options program-options (rest (vector->list sys.argv)))
               except
               (:Getopt/MissingArg _ _) -> (usage)
               (:Getopt/UnknownArg _ _) -> (usage)))
        (verbose? #f)
        (dump? #f)
        (paths (list:nil))
        (ders (list:nil))
        (now (let ((t (gettime/realtime))) t.sec)))
    (when-maybe v (get-bool-opt opts 'h) (usage))
    (when-maybe v (get-bool-opt opts 'v) (set! verbose? v))
    (when-maybe v (get-bool-opt opts 'a) (set! dump? v))
    (match (getopt opts 'files) with
      (maybe:yes (optval:list vals))
      -> (for-list val vals
           (match val with
             (optval:string path) -> (push! paths path)
             _ -> #u))
      _ -> (usage))
    (for-list path (reverse paths)
      (for-list der (read-certificates path)
        (push! ders der)))
    (let ((certs (list->vector (map parse-x509 (reverse ders))))
          (n (vector-length certs)))
      (for-range i n
        (let ((x certs[i]))
          (printf (if (= i 0) "" "\n")
                  (ansi (bold blue) "-- [" (int i) "] " (x509-name-short x.cert.subject) " --")
                  "\n")
          (print-x509 x now verbose?)
          (when dump?
            (printf (bold "asn.1"))
            (pp-ber (ber->asn1 x.der) 1)
            (printf "\n"))))
      (let ((problems (check-chain certs now)))
        (%exit #f (if (= problems 0) 0 1))))
    ))

(main)
