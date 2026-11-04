;; -*- Mode: Irken -*-

;; a small ACME (rfc8555) client, for getting certificates from
;;  Let's Encrypt with http-01 challenges.
;;
;; $ demo/acme/acme -challenges /var/acme dark.nightmare.com nightmare.com
;;
;; the challenge responses are written to the -challenges directory,
;;  as files named by their tokens: a web server must answer
;;  http://<name>/.well-known/acme-challenge/<token> with them (see
;;  serve -acme in demo/h2).
;;
;; the account key and the certificate's key are P-256 keys, created
;;  as needed and kept (PKCS#8 PEM).  with -days, nothing is done while
;;  the existing certificate has more than that many days left.

(require "lib/basis.scm")
(require "lib/getopt.scm")
(require "lib/urandom.scm")
(require "lib/os.scm")
(require "lib/cmap.scm")
(require "lib/counter.scm")
(require "lib/codecs/hex.scm")
(require "lib/parse/lexer.scm")
(require "lib/dfa/charset.scm")
(require "lib/dfa/rx.scm")
(require "lib/dfa/deriv.scm")
(require "lib/dfa/dfa.scm")
(require "lib/dfa/emit.scm")
(require "lib/dfa/lexicon.scm")
(require "lib/parse/earley.scm")
(require "lib/parse/parser.scm")
(require "lib/json.scm")
(require "lib/crypto/jose.scm")
(require "lib/crypto/csr.scm")
(require "doom/http/client.scm")

(define letsencrypt "https://acme-v02.api.letsencrypt.org/directory")
(define letsencrypt-staging "https://acme-staging-v02.api.letsencrypt.org/directory")

(define program-options
  (makeopt
   (arg 'server (string ""))
   (flag 'staging)
   (arg 'root (string ""))
   (arg 'account (string "account.key"))
   (arg 'key (string "key.pem"))
   (arg 'cert (string "cert.pem"))
   (arg 'challenges (string ""))
   (arg 'email (string ""))
   (arg 'profile (string ""))
   (arg 'days (int 0))
   (flag 'v)
   (flag 'h)
   (pos+ 'names (string ""))
   ))

(define (usage)
  (printf "get a certificate from an ACME CA (Let's Encrypt), with http-01.\n\n"
          "Usage: " sys.argv[0] " -challenges <dir> [options] name ...\n"
          "  -challenges <dir>  where to write challenge responses: serve these\n"
          "                     at http://<name>/.well-known/acme-challenge/\n"
          "  [-account <file>]  the account key [account.key], created if missing\n"
          "  [-key <file>]      the certificate's key [key.pem], created if missing\n"
          "  [-cert <file>]     where to write the certificate chain [cert.pem]\n"
          "  [-days <n>]        do nothing if -cert is good for more than <n> days\n"
          "  [-email <addr>]    contact address for the account\n"
          "  [-profile <name>]  certificate profile (e.g. shortlived)\n"
          "  [-staging]         use Let's Encrypt's staging server\n"
          "  [-server <url>]    use this ACME directory URL\n"
          "  [-root <pem>]      trust these roots for the ACME server\n"
          "  [-v]               print the protocol exchange\n"
          "  [-h]               print this usage.\n")
  (%exit #f 2))

(define (die msg)
  (printf "acme: " msg "\n")
  (%exit #f 1))

(define *verbose* #f)

(defmacro vprintf
  (vprintf item ...) -> (when *verbose* (printf item ...)))

;; --- keys ---

(define (load-or-make-key path RNG what)
  (if (isfile? path)
      (match (read-skey path) with
        (skey:p256 sk) -> sk
        _ -> (die (format path ": not a P-256 key")))
      (let ((sk (p256-make-key RNG)))
        (write-pem path "PRIVATE KEY" (asn1->ber (p256-pkcs8 sk)) #o600)
        (printf "created " what " " path "\n")
        sk)))

;; --- an ACME session ---

(define (acme/make directory-url anchors sk RNG)

  (let ((nonce "")
        (kid "")
        (dir (json:null))
        (jwk (jwk-p256 (p256-public-key sk)))
        (headers (list (:tuple "user-agent" "irken-acme/0.1"))))

    (define (header name resp)
      (match (h2/header name resp.headers) with
        (maybe:yes v) -> v
        (maybe:no) -> ""))

    (define (parse-json resp)
      (try
       (json/parse-string resp.body)
       except
       (:Json/UnicodeString) -> (raise (:ACME/Error "client" "unicode in a JSON string"))
       ;; XXX the parser raises various exceptions on bad input.
       ))

    (define (dir-url name)
      (let ((url (json/get-string dir name)))
        (if (= 0 (string-length url))
            (raise (:ACME/Error "client" (format "no " name " in the directory")))
            url)))

    (define (request method url hs body)
      (vprintf "> " method " " url "\n")
      (let ((resp (https/request method url (append headers hs) body anchors))
            (n (header "replay-nonce" resp)))
        (vprintf "< " resp.status " " (string resp.body) "\n")
        (when (> (string-length n) 0)
          (set! nonce n))
        resp))

    (define (fresh-nonce)
      (request "HEAD" (dir-url "newNonce") (list:nil) "")
      nonce)

    ;; POST a JWS of <payload> (a string: "" for POST-as-GET).
    ;;  retries on badNonce; raises (:ACME/Error type detail) on error.
    (define (post url payload)
      (let loop ((tries 0))
        (let ((n (if (= 0 (string-length nonce)) (fresh-nonce) nonce))
              (_ (set! nonce ""))
              (protected (json:obj
                          (list (if (= 0 (string-length kid))
                                    (:tuple "jwk" jwk)
                                    (:tuple "kid" (json:string kid)))
                                (:tuple "nonce" (json:string n))
                                (:tuple "url" (json:string url)))))
              (jws (jws-es256 sk RNG protected payload))
              (resp (request "POST" url
                             (list (:tuple "content-type" "application/jose+json"))
                             (json->string jws))))
          (if (eq? (string-ref resp.status 0) #\2)
              resp
              (let ((problem (try
                              (parse-json resp)
                              except
                              (:ACME/Error _ _) -> (json:null)))
                    (type (json/get-string problem "type"))
                    (detail (json/get-string problem "detail")))
                (if (and (string=? type "urn:ietf:params:acme:error:badNonce") (< tries 5))
                    (loop (+ tries 1))
                    (raise (:ACME/Error (if (= 0 (string-length type)) resp.status type)
                                        (if (= 0 (string-length detail)) (repr-string resp.body) detail)))))))))

    (define (post-json url payload)
      (parse-json (post url (json->string payload))))

    ;; POST-as-GET
    (define (fetch url)
      (post url ""))

    (define (fetch-json url)
      (parse-json (fetch url)))

    (define (open)
      (let ((resp (request "GET" directory-url (list:nil) "")))
        (when (not (string=? resp.status "200"))
          (raise (:ACME/Error resp.status (format "can't get the directory " directory-url))))
        (set! dir (parse-json resp))))

    (define (login email)
      (let ((resp (post (dir-url "newAccount")
                        (json->string
                         (json:obj
                          (append (list (:tuple "termsOfServiceAgreed" (json:bool #t)))
                                  (if (= 0 (string-length email))
                                      (list:nil)
                                      (list (:tuple "contact" (json:array (list (json:string (format "mailto:" email))))))))))))
            (loc (header "location" resp)))
        (when (= 0 (string-length loc))
          (raise (:ACME/Error "client" "no account URL (Location) from newAccount")))
        (set! kid loc)
        (vprintf "account " kid (if (string=? resp.status "201") " (new)" "") "\n")
        loc))

    ;; -> {url order}
    (define (new-order names profile)
      (let ((resp (post (dir-url "newOrder")
                        (json->string
                         (json:obj
                          (append
                           (list (:tuple "identifiers"
                                         (json:array (map (lambda (name)
                                                            (json:obj (list (:tuple "type" (json:string "dns"))
                                                                            (:tuple "value" (json:string name)))))
                                                          names))))
                           (if (= 0 (string-length profile))
                               (list:nil)
                               (list (:tuple "profile" (json:string profile))))))))))
        {url=(header "location" resp) order=(parse-json resp)}))

    (define (thumbprint) (jwk-thumbprint jwk))

    {open=open login=login new-order=new-order post-json=post-json
     fetch=fetch fetch-json=fetch-json thumbprint=thumbprint header=header}
    ))

;; --- the steps ---

(define (token-ok? token)
  (and (> (string-length token) 0)
       (every? (lambda (ch) (or (alphanumeric? ch) (char=? ch #\-) (char=? ch #\_)))
               (string->list token))))

(define (alphanumeric? ch)
  (or (digit? ch) (letter? ch)))

(define (write-file path data mode)
  (let ((f (file/open-write path #t mode)))
    (file/write f data)
    (file/flush f)
    (file/close f)))

(define (string-member key j)
  (json/get-string j key))

;; wait for <url> (an authorization or order) to leave <pending>
;;  states, polling every couple of seconds.
(define (poll acme url pending)
  (let loop ((tries 0))
    (let ((j (acme.fetch-json url))
          (status (json/get-string j "status")))
      (if (and (member? status pending string=?) (< tries 60))
          (begin
            (timeq/sleep 2000)
            (loop (+ tries 1)))
          j))))

(define (challenge-error authz)
  (let ((r "validation failed"))
    (for-list ch (json/get-array authz "challenges")
      (match (json/get ch "error") with
        (maybe:yes e) -> (set! r (format (json/get-string e "type") ": " (json/get-string e "detail")))
        (maybe:no) -> #u))
    r))

;; complete the http-01 challenge for each pending authorization.
(define (authorize acme order challenge-dir)
  (let ((todo (list:nil)))
    ;; write all the responses, then ask for validation of each.
    (for-list a (json/get-array order "authorizations")
      (match a with
        (json:string url)
        -> (let ((authz (acme.fetch-json url))
                 (name (json/get-string (match (json/get authz "identifier") with
                                          (maybe:yes i) -> i
                                          _ -> (json:null))
                                        "value")))
             (if (string=? (json/get-string authz "status") "valid")
                 (printf name ": already authorized\n")
                 (let ((http01 (filter (lambda (ch) (string=? (json/get-string ch "type") "http-01"))
                                       (json/get-array authz "challenges"))))
                   (match http01 with
                     () -> (raise (:ACME/Error "client" (format "no http-01 challenge for " name)))
                     (ch . _)
                     -> (let ((token (json/get-string ch "token"))
                              (path (format challenge-dir "/" token)))
                          (when (not (token-ok? token))
                            (raise (:ACME/Error "client" (format "bad token " (string token)))))
                          (write-file path (format token "." (acme.thumbprint)) #o644)
                          (push! todo {name=name url=url chall=(json/get-string ch "url") path=path}))))))
        _ -> #u))
    (for-list t todo
      (printf t.name ": validating...\n")
      (acme.post-json t.chall (json:obj (list:nil))))
    ;; wait for them all, then remove the files, then report.
    (let ((failures (list:nil)))
      (for-list t todo
        (let ((authz (poll acme t.url '("pending" "processing"))))
          (if (string=? (json/get-string authz "status") "valid")
              (printf t.name ": authorized\n")
              (push! failures (format t.name ": " (challenge-error authz))))))
      (for-list t todo
        (unlink t.path))
      (when (not (null? failures))
        (raise (:ACME/Error "authorization" (format (join "; " (reverse failures)))))))))

;; read the PEM chain in <pem>.
(define (pem-certs pem)
  (let ((r (list:nil)))
    (for section (pem-read-gen (string-generator pem))
      (match section with
        (:tuple "CERTIFICATE" der) -> (push! r der)
        _ -> #u))
    (reverse r)))

;; does the existing certificate cover <names> for more than <days>?
(define (still-good? cert-path names days now)
  (and (> days 0)
       (isfile? cert-path)
       (let ((x (parse-x509 (first (read-certificates cert-path)))))
         (and (every? (lambda (name) (x509-valid-for-host? x.cert name)) names)
              (> (- (x509-time->unix x.cert.validity.not-after) now) (* days 86400))))))

(define (get-certificate opts)
  (let ((names (match (getopt opts 'names) with
                 (maybe:yes (optval:list vals))
                 -> (map (lambda (v) (match v with (optval:string s) -> (downcase s) _ -> "")) vals)
                 _ -> (usage)))
        (str (lambda (name default)
               (match (get-string-opt opts name) with
                 (maybe:yes v) -> (if (> (string-length v) 0) v default)
                 _ -> default)))
        (server (if (match (get-bool-opt opts 'staging) with (maybe:yes v) -> v _ -> #f)
                    letsencrypt-staging
                    (str 'server letsencrypt)))
        (challenge-dir (str 'challenges ""))
        (cert-path (str 'cert "cert.pem"))
        (key-path (str 'key "key.pem"))
        (days (match (get-int-opt opts 'days) with (maybe:yes n) -> n _ -> 0))
        (now (let ((t (gettime/realtime))) t.sec)))
    (when (= 0 (string-length challenge-dir))
      (usage))
    (when (not (isdir? challenge-dir))
      (die (format "no such directory: " challenge-dir)))
    (if (still-good? cert-path names days now)
        (printf cert-path " is good for more than " (int days) " days: nothing to do.\n")
        (let ((RNG (urandom-make))
              (anchors (load-trust-anchors (str 'root "")))
              (account-key (load-or-make-key (str 'account "account.key") RNG "account key"))
              (acme (acme/make server anchors account-key RNG)))
          (acme.open)
          (acme.login (str 'email ""))
          (let ((o (acme.new-order names (str 'profile "")))
                (order o.order))
            (when (not (string=? (json/get-string order "status") "ready"))
              (authorize acme order challenge-dir))
            (let ((cert-key (load-or-make-key key-path RNG "certificate key"))
                  (csr (p256-csr names cert-key RNG))
                  (_ (acme.post-json (json/get-string order "finalize")
                                     (json:obj (list (:tuple "csr" (json:string (b64url-encode csr)))))))
                  (done (poll acme o.url '("pending" "ready" "processing"))))
              (when (not (string=? (json/get-string done "status") "valid"))
                (raise (:ACME/Error "order" (format "order is " (json/get-string done "status")))))
              (let ((pem (acme.fetch (json/get-string done "certificate")))
                    (ders (pem-certs pem.body))
                    (leaf (if (null? ders)
                              (raise (:ACME/Error "certificate" "no certificates in the response"))
                              (parse-x509 (first ders)))))
                ;; make sure it's what we asked for.
                (match leaf.cert.pubkey.key with
                  (public-key:ec k _)
                  -> (when (not (string=? k (p256-public-key cert-key)))
                       (raise (:ACME/Error "certificate" "the certificate is for a different key")))
                  _ -> (raise (:ACME/Error "certificate" "the certificate is not for a P-256 key")))
                (for-list name names
                  (when (not (x509-valid-for-host? leaf.cert name))
                    (raise (:ACME/Error "certificate" (format "the certificate doesn't name " name)))))
                (write-file cert-path pem.body #o644)
                (printf "wrote " cert-path ": " (int (length ders)) " certificates, for "
                        (join ", " (let ((alt (x509-alt-names leaf.cert))) alt.dns))
                        ", valid until " (x509-time-repr leaf.cert.validity.not-after) "\n"))))))
    (%exit #f 0)))

(define (main)
  (set-verbose-gc #f)
  (let ((opts (try
               (process-options program-options (rest (vector->list sys.argv)))
               except
               (:Getopt/MissingArg _ _) -> (usage)
               (:Getopt/UnknownArg _ _) -> (usage))))
    (when-maybe v (get-bool-opt opts 'h) (usage))
    (when-maybe v (get-bool-opt opts 'v) (set! *verbose* v))
    (poller/fork
     (lambda ()
       (try
        (get-certificate opts)
        except
        (:ACME/Error type detail) -> (die (format type ": " detail))
        (:HTTPS/Failed why) -> (die why)
        (:OSError n) -> (die (format "error: " (strerror n)))
        (:X509/Invalid why) -> (die why))))
    (poller/wait-and-schedule)))

(main)
