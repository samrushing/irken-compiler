;; -*- Mode: Irken -*-

;; auth methods:
;;
;; user
;; publickey
;; host

(define (build-userauth-suggest methods)
  (msg-pack
   (ssh-mtype:userauth-failure)
   (ssh-data
    (auths (sshD:namelist methods))
    (partial (sshD:bool #f)))))

(define (build-userauth-failure)
  (msg-pack
   (ssh-mtype:userauth-failure)
   (ssh-data
    (auths (sshD:namelist (list:nil)))
    (partial (sshD:bool #f)))))

(define (build-userauth-success)
  (msg-pack (ssh-mtype:userauth-success) (ssh-data)))

(define (make-password-authenticator user-map) ;; (tree string string) user->password

  (define (attempt sid user service extra)
    (let ((pkt (ssh-unpack-with-descriptor
                extra 0
                (ssh-pkt (change bool) (password s))))
          (change? (ssh-pkt/get-bool pkt 'change))
          (password (ssh-pkt/get-string pkt 'password)))
      (printf "password auth : user = " (string user)
              " pass = " (string password) "\n")
      (if change?
          (:tuple #f (maybe:no))
          (match (tree/member user-map string-cmp user) with
            (maybe:yes pwd)
            -> (if (string=? pwd password) ;; XXX CT
                   (:tuple #t (maybe:no))
                   (:tuple #f (maybe:no)))
            (maybe:no)
            -> (:tuple #f (maybe:no))
            ))))

  { name = "password"
    attempt = attempt }

  )

(define (build-pk-ok alg blob)
  (msg-pack
   (ssh-mtype:userauth-pk-ok)
   (ssh-data (alg (sshD:string alg)) (pkey (sshD:string blob)))))

(define (make-publickey-authenticator pkey-map) ;; (tree string (tree string string)) user->alg->pkey

  ;; boxes within boxes...
  (define (unpack-pkey pkey)
    (let ((pkt (ssh-unpack-with-descriptor pkey 0 (ssh-pkt (alg s) (pkey s)))))
      (ssh-pkt/get-string pkt 'pkey)))

  (define (unpack-sig sig)
    (let ((pkt (ssh-unpack-with-descriptor sig 0 (ssh-pkt (alg s) (sig s)))))
      (ssh-pkt/get-string pkt 'sig)))

  (define (check-sig sid user service keys alg sig)

    (define (build-tbs pkey)
      (rope->string
       (packer
        (ssh-data
         (sid (sshD:string sid))
         (msg (sshD:byte1 50)) ;; MSG_USERAUTH_REQUEST
         (usr (sshD:string user))
         (srv (sshD:string service))
         (met (sshD:string "publickey"))
         (yes (sshD:bool #t))
         (alg (sshD:string alg))
         (pk  (sshD:string pkey))))))

    (match (tree/member keys string-cmp alg) with
      (maybe:no) -> (:tuple #f (maybe:no))
      (maybe:yes pkey)
      -> (let ((tbs (build-tbs pkey))
               (ver? (ed25519-verify (unpack-sig sig) tbs (unpack-pkey pkey))))
           ;; (printf " tbs " (string->hex tbs) "\n")
           ;; (printf " sig " (string->hex sig) "\n")
           ;; (printf "--------- ed25519 pkey verify => " (bool ver?) "\n")
           (:tuple ver? (maybe:no)))
      ))

  (define (attempt sid user service extra)
    (let ((pkt (ssh-unpack-with-descriptor
                extra 0
                (ssh-pkt (sig? bool) (alg s) (pkey s))))
          (sig? (ssh-pkt/get-bool pkt 'sig?))
          (alg (ssh-pkt/get-string pkt 'alg))
          (pkey (ssh-pkt/get-string pkt 'pkey)))
      ;; (printf "publickey auth : user = " (string user)
      ;;         " alg = " (string alg) "\n"
      ;;         " pkey = " (string->hex pkey) "\n")
      (match (tree/member pkey-map string-cmp user) with
        (maybe:no)
        -> (:tuple #f (maybe:no))
        (maybe:yes keys)
        -> (if sig?
               (let ((extra (ssh-pkt/get-string pkt 'extra))
                     (epkt (ssh-unpack-with-descriptor extra 0 (ssh-pkt (sig s))))
                     (sig (ssh-pkt/get-string epkt 'sig)))
                 (check-sig sid user service keys alg sig))
               (:tuple #f (maybe:yes (build-pk-ok alg pkey))))
        )))
  { name = "publickey"
    attempt = attempt }
  )

(define (attempt-authentication auths pkt session-id)
  (let ((user (ssh-pkt/get-string pkt 'user))
        (service (ssh-pkt/get-string pkt 'service))
        (method (ssh-pkt/get-string pkt 'method)))
    (match service with
      "ssh-connection"
      -> (let loop ((auths auths))
           (match auths with
             ()
             -> (:tuple #f (build-userauth-suggest (map (lambda (x) x.name) auths)))
             (auth . tl)
             -> (if (string=? auth.name method)
                    (match (auth.attempt session-id user service (ssh-pkt/get-string pkt 'extra)) with
                      (:tuple #t _)                 -> (:tuple #t (build-userauth-success))
                      (:tuple #f (maybe:no))        -> (:tuple #f (build-userauth-failure))
                      (:tuple #f (maybe:yes reply)) -> (:tuple #f reply))
                    (loop tl))))
      other
      -> (begin
           (printf "unknown service " (string service) "\n")
           (:tuple #f (build-userauth-failure)))
      )))
