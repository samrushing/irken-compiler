;; -*- Mode: Irken -*-

(define (ssh-transport sock)

  (let ((buf "")
        (blen 0)
        (pos 0)
        (session-id "")
        (sequence-id 0)
        (sequence-counter-recv 0)
        (sequence-counter-send 0)
        (cipher none-cipher)
        (codec-send (none-cipher-make-codec "" "" ""))
        (codec-recv (none-cipher-make-codec "" "" ""))
        )

    (define (assure n)
      (when (> n (- blen pos))
        (let ((data (sock.recv))
              (dlen (string-length data)))
          (when (= dlen 0)
            (raise (:SSH/EOF)))
          (cond ((> blen pos)
                 (set! buf (string-append (substring buf pos blen) data))
                 (set! pos 0)
                 (set! blen (string-length buf)))
                ((= blen pos)
                 (set! buf data)
                 (set! pos 0)
                 (set! blen dlen)))
          (assure n))))

    (define (bump x n)
      (inc! pos n)
      x)

    (define (getch)
      (assure 1)
      (bump (string-ref buf pos) 1))

    ;; XXX accept only printable chars as per spec?
    ;; XXX length limit
    (define (get-banner)
      (let loop ((cr? #f)
                 (r '()))
        (match cr? (getch) with
          #f #\return  -> (loop #t r)
          #f       ch  -> (loop #f (list:cons ch r))
          #t #\newline -> (list->string (reverse r)) ;; done
          #t       ch  -> (loop #f (list:cons ch (list:cons #\return r))) ;; orphan CR
          )))

    (define (get-u8)
      (assure 1)
      (bump (char->int (string-ref buf pos)) 1))

    (define (get-u32)
      (assure 4)
      (let ((r 0))
        (for-range i 4
          (set! r (logior (<< r 8) (char->int (string-ref buf (+ pos i))))))
        (bump r 4)))

    (define (get-string n)
      (assure n)
      (bump (substring buf pos (+ pos n)) n))

    ;; XXX asymmetry between encrypt/decrypt: one takes seq, the others take nonce.
    (define (get-packet)
      (let ((header (get-string 4))
            (nonce (int->u64->be sequence-counter-recv))
            (plen (be->int (codec-recv.decrypt-header header nonce)))
            (ct (get-string plen))
            (tag (get-string cipher.tag-size))
            (pt (codec-recv.decrypt-packet header ct nonce tag))
            (padlen (char->int (string-ref pt 0)))
            (payload (substring pt 1 (- plen padlen))))
        (inc! sequence-counter-recv)
        payload))

    (define (send* payload)
      (let ((paylen (rope-length payload))
            (block-size (max 8 cipher.block-size))
            (extra-size (if cipher.encrypted-length 1 5))
            (padlen0 (- block-size (mod (+ paylen extra-size) block-size)))
            (padlen1 (if (< padlen0 4) (+ padlen0 block-size) padlen0))
            (padding (rope:leaf (ssh/RNG padlen1)))
            (header (u32->be (+ paylen padlen1 1)))
            (pkt (rope/build (rope:leaf (u8->be padlen1)) payload padding))
            (raw (rope->string pkt))
            ((ct tag) (codec-send.encrypt raw sequence-counter-send))
            (all (if cipher.encrypted-length
                     (string-append ct tag)
                     (string-append header ct tag))))
        ;; (printf ">>> paylen " (int paylen) "\n"
        ;;         "   padlen0 " (int padlen0) "\n"
        ;;         "   padlen1 " (int padlen1) "\n"
        ;;         "    header " (string header) "\n"
        ;;         "       all " (string->hex all) "\n")
        (sock.send all)
        (inc! sequence-counter-send)
        ))

    (define (send payload)
      (let ((pkt (ssh-unpack* (rope->string payload) ">>> ")))
        (send* payload)))

    (define (set-session-id sid)
      (set! session-id sid))

    (define (newkeys name eph-key exchange-hash)
      (let ((cipher-desc (get-cipher-by-name name))
            (keys (gen-keys cipher-desc eph-key exchange-hash session-id)))
        (set! cipher cipher-desc)
        (set! codec-send (cipher-desc.codec keys.s2c-key keys.s2c-iv keys.s2c-mac))
        (set! codec-recv (cipher-desc.codec keys.c2s-key keys.c2s-iv keys.c2s-mac))
        (printf "set new keys\n")
        ))

    { get-packet = get-packet
      get-banner = get-banner
      send = send
      set-session-id = set-session-id
      newkeys = newkeys
      }
    ))

;; transport state
(datatype ssh-tstate
  (:init)
  (:prekex)
  (:kexed)
  (:keyed)
  (:closed)
  )

(define ssh-tstate->name
  (ssh-tstate:init)   -> 'init
  (ssh-tstate:prekex) -> 'prekex
  (ssh-tstate:kexed)  -> 'kexed
  (ssh-tstate:keyed)  -> 'keyed
  (ssh-tstate:closed) -> 'closed
  )

;; auth state
(datatype ssh-astate
  (:init)
  (:requested) ;; userauth-requested
  (:user)      ;; method user
  (:pkey)      ;; method public-key
  (:host)      ;; method host
  (:authed)    ;; auth'd, somehow
  )

(define (ssh-conn-loop sockfun authenticators session-handler)
  (let ((s (sockfun 8192 8192))
        (trans (ssh-transport s))
        (sbanner "SSH-2.0-irkenDOOM")
        (cbanner "")
        (kexinit-s2c-raw "")
        (kexinit-c2s-raw "")
        (shared-secret "")
        (exchange-hash "")
        (session-id "")
        (tstate (ssh-tstate:init))
        (astate (ssh-astate:init))
        (quit #f)
        (chan-man (make-channel-manager))
        )
    (s.send (string-append sbanner "\r\n")) ;; note: not on chan
    (set! cbanner (trans.get-banner))
    (printf (string cbanner) "\n")

    (define (handle-kexinit payload)
      (let ((pkt (ssh-unpack payload))
            (kexinit-s2c-pkt (msg-pack (ssh-mtype:kexinit) (make-kexinit))))
        ;; ummmm... actually DO the kexinit-choice algorithm lol
        (trans.send kexinit-s2c-pkt)
        (set! kexinit-c2s-raw payload)
        (set! kexinit-s2c-raw (rope->string kexinit-s2c-pkt))
        (set! tstate (ssh-tstate:prekex))))

    (define (maybe-set-session-id sid)
      (when (zero? (string-length session-id))
        (trans.set-session-id sid)
        (set! session-id sid)))

    (define (handle-ecdh-init payload)
      (let ((ecdh-c2s-init-pkt (ssh-unpack payload))
            (skey (ssh/RNG 32)) ;; is this modified?
            (pkey (curve25519-scalarmult-base skey))
            (q-c (ssh-pkt/get-string ecdh-c2s-init-pkt 'q-c))
            (eph-k (curve25519-scalarmult skey q-c))
            (host-pkey (pack-public-key server-host-pkey))
            (tbh (build-ecdh-hash
                  cbanner sbanner
                  kexinit-c2s-raw kexinit-s2c-raw
                  host-pkey
                  q-c
                  pkey
                  eph-k))
            (tbs (sha256 tbh)) ;; exchange-hash
            (sig (pack-signature (ed25519-sign tbs server-host-skey)))
            (kex-ecdh-reply-pkt (build-ecdh-reply host-pkey pkey sig)))
        (trans.send kex-ecdh-reply-pkt)
        (maybe-set-session-id tbs)
        (set! exchange-hash tbs)
        (set! shared-secret eph-k)
        (set! tstate (ssh-tstate:kexed))
        ))

    (define (handle-newkeys payload)
      (trans.send (msg-pack (ssh-mtype:newkeys) (alist:nil)))
      ;; XXX choice of cipher is normally made in KEXINIT processing.
      (trans.newkeys "chacha20-poly1305@openssh.com" (string->mpint shared-secret) exchange-hash)
      ;;(trans.newkeys "aes256-gcm@openssh.com" (string->mpint eph-k) tbs)
      (set! tstate (ssh-tstate:keyed)))

    (define (handle-servreq payload authenticated?)
      (let ((servreq-pkt (ssh-unpack payload))
            (servname (ssh-pkt/get-string servreq-pkt 'name)))
        (cond ((string=? servname "ssh-userauth")
               (trans.send (msg-pack
                           (ssh-mtype:service-accept)
                           (ssh-data (name (sshD:string servname)))))
               (set! astate (ssh-astate:requested)))
              ((and (string=? servname "ssh-connection") authenticated?)
               (trans.send (msg-pack
                           (ssh-mtype:service-accept)
                           (ssh-data (name (sshD:string servname)))))
               (set! astate (ssh-astate:requested))))))


    (define (handle-disconnect payload)
      (let ((pkt (ssh-unpack payload))
            (reason (ssh-pkt/get-u32 pkt 'reason))
            (description (ssh-pkt/get-string pkt 'description)))
        (printf (bold "disconnected:") " [" (int reason) "] " (string description) "\n")
        (set! quit #t)
        ))

    (define (handle-debug payload)
      (let ((pkt (ssh-unpack payload))
            (display? (ssh-pkt/get-bool pkt 'always-display))
            (message (ssh-pkt/get-string pkt 'message))
            (language (ssh-pkt/get-string pkt 'language)))
        (printf (bold "[ssh-debug:] ") " " (string message) "\n")
        ))

    (define (tstate-transition kind payload)
      ;;(printf "tstate " (sym (ssh-tstate->name tstate)) " kind " (sym (ssh-mtype->name kind)) "\n")
      (match tstate kind with
        _                   (ssh-mtype:disconnect)    -> (handle-disconnect payload)
        _                   (ssh-mtype:ignore)        -> #u
        _                   (ssh-mtype:debug)         -> (handle-debug payload)
        (ssh-tstate:init)   (ssh-mtype:kexinit)       -> (handle-kexinit payload)
        (ssh-tstate:prekex) (ssh-mtype:kex-ecdh-init) -> (handle-ecdh-init payload)
        (ssh-tstate:kexed)  (ssh-mtype:newkeys)       -> (handle-newkeys payload)
        (ssh-tstate:keyed) _ -> (astate-transition kind payload)
        _ _ -> (set! quit #t)
        ))

    (define (handle-authreq payload)
      (let ((authreq (ssh-unpack payload))
            ((good? reply) (attempt-authentication authenticators authreq session-id)))
        (trans.send reply)
        (when good?
          (set! astate (ssh-astate:authed)))
        ))

    (define (astate-transition kind payload)
      (match astate kind with
        (ssh-astate:init)      (ssh-mtype:service-request)  -> (handle-servreq payload #f)
        (ssh-astate:requested) (ssh-mtype:userauth-request) -> (handle-authreq payload)
        (ssh-astate:authed)    _                            -> (handle-authed-packet kind payload)
        _ _ -> (set! quit #t)
        ))

    (define (channel-write-data rchan data)
      (trans.send (build-channel-data rchan data))
      )

    (define (handle-session-open schan winsz maxpkt)
      (let ((chan (chan-man.new schan winsz maxpkt session-handler channel-write-data trans.send)))
        (trans.send (build-channel-open-yes chan.s2c-id chan.c2s-id winsz maxpkt))
        ))

    (define (handle-channel-open pkt)
      (let ((chan-type (ssh-pkt/get-string pkt 'type))
            (c2s-chan (ssh-pkt/get-u32 pkt 'schan))
            (winsz (ssh-pkt/get-u32 pkt 'winsz))
            (maxpkt (ssh-pkt/get-u32 pkt 'max)))
        (match chan-type with
          "session" -> (handle-session-open c2s-chan winsz maxpkt)
          ;; other types are x11, forwarding, command, sftp, etc.
          _ -> (trans.send
                (build-channel-open-no
                 c2s-chan
                 (ssh-open:unknown-channel-type)
                 (format "unknown channel type: " (string chan-type))
                 "en"))
          )))

    (define (handle-channel-data pkt)
      (let ((rchan (ssh-pkt/get-u32 pkt 'rchan))
            (data (ssh-pkt/get-string pkt 'data)))
        (match (chan-man.lookup rchan) with
          (maybe:yes chan) -> (chan.recv data)
          (maybe:no)       -> (raise (:SSH/Error "data for unknown channel")) ;; XXX do better
          )))

    (define (handle-channel-request pkt)
      (let ((rchan (ssh-pkt/get-u32 pkt 'rchan))
            (type (ssh-pkt/get-string pkt 'type))
            (want (ssh-pkt/get-bool pkt 'want))
            (extra (ssh-pkt/extra pkt)))
        (match (chan-man.lookup rchan) with
          (maybe:yes chan) -> (chan.request type want extra)
          (maybe:no)       -> (raise (:SSH/Error "data for unknown channel")) ;; XXX do better
          )))

    (define (handle-authed-packet kind payload)
      (let ((pkt (ssh-unpack payload)))
        (match kind with
          (ssh-mtype:channel-open)    -> (handle-channel-open pkt)
          (ssh-mtype:channel-data)    -> (handle-channel-data pkt)
          (ssh-mtype:channel-request) -> (handle-channel-request pkt)
          (ssh-mtype:kexinit)         -> (handle-kexinit payload) ;; rekey
          _ -> #u)
        ))

    (let loop ()
      (let ((payload (trans.get-packet))
            (kind (code->msg-type (char->int (string-ref payload 0)))))
        (tstate-transition kind payload)
        (if (not quit)
            (loop)
            (printf "exiting ssh packet loop...\n"))
        ))
    ))
