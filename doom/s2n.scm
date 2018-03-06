;; -*- Mode: Irken -*-

;; back to the 'object/inheritance' problem.
;; we need to use a 'doom' socket to accept
;;  connections, because accept() will otherwise
;;  block.  But the type of connection returned/created
;;  needs to be an s2n conn.  We want s2n & doom sockets
;;  to be interchangeable.  SO - do we:
;;
;; 1) re-implement most of doom logic here?
;;    [by 'logic' I mean stuff like recv-exact].
;; 2) try to inherit in some way?

;; I think we need #1.  There's literally nothing to be gained
;;    from #2.
;;
;; another question: can we hide 'negotiate' completely?
;; seems easy with `connect`.  `accept` is another matter,
;; since we don't want to negotiate in the listening thread.
;; it would suck to check a 'negotiated' flag in every send/recv,
;; but is there any other way?  Maybe accept returns a thunk?

;; XXX need to implement S2N_SELF_SERVICE_BLINDING, otherwise
;;   s2n will insert random delays after errors.

(require-ffi 'libc)
(require-ffi 'socket)
(require-ffi 's2n)

(define (s2n-init) (s2n/s2n_init))

(define (s2n-cleanup) (s2n/s2n_cleanup))

(define (s2n-strerror n)
  (let ((s* (s2n/s2n_strerror n (cstring "EN")))
        (slen (libc/strlen s*)))
    (%cref->string #f s* slen)))

(define (get-s2n-error)
  (let ((err (%c-get-int int s2n/s2n_errno))
        (etype (s2n/s2n_error_get_type err)))
    (:S2NError etype err (s2n-strerror err))))

(define (format-s2n-error)
  (match (get-s2n-error) with
    (:S2NError etype err errstr)
    -> (format "type=" (int etype) " code = " (hex err) ": " (string errstr))))

(define (trys2n val)
  (if (not (= 0 val))
      (raise (get-s2n-error))
      val))

;; --------------------------------------------------------------------------------
;;                            config object
;; --------------------------------------------------------------------------------

(define (s2n-make-config)

  (let ((config (s2n/s2n_config_new)))

    (define (set-cipher-preferences version)
      (s2n/s2n_config_set_cipher_preferences config (cstring version)))

    (define (set-protocol-preferences protos)
      (let ((n (length protos))
            (cprotos (map cstring protos))
            (cprotos* (halloc (* char) n)))
        (for-range i n
          (c-set-ptr (c-aref cprotos* i) (nth cprotos i)))
        (s2n/s2n_config_set_protocol_preferences config (c-aref cprotos* 0) n)
        ))

    (define (add-cert-chain-and-key chain-pem skey-pem)
      (let ((chain (cstring chain-pem))
            (skey (cstring skey-pem)))
        (s2n/s2n_config_add_cert_chain_and_key config chain skey)
        ))

    (define (add-dhparams dhparams-pem)
      (s2n/s2n_config_add_dhparams config (cstring dhparams-pem)))

    (define (get) config)

    {
     set-cipher-preferences   = set-cipher-preferences
     set-protocol-preferences = set-protocol-preferences
     add-cert-chain-and-key   = add-cert-chain-and-key
     add-dhparams             = add-dhparams
     get                      = get
     }

    ))

;; --------------------------------------------------------------------------------
;;                          connection object
;; --------------------------------------------------------------------------------

(defmacro s2n/make-sock
  (s2n/make-sock config mode sock)
  -> (s2n/make-conn config mode sock 8192 8192)
  (s2n/make-sock config mode sock isize osize)
  -> (s2n/make-conn config mode sock isize osize)
  )

(define (s2n/make-conn config mode sock isize osize)

  (let ((conn* (s2n/s2n_connection_new mode))
        (ibuf (buffer/make isize))
        (obuf (buffer/make osize))
        (closed #f))

    (define (wait-on-block blocked)
      ;;(printf "wait on " (int blocked) "\n")
      (if (= blocked S2N_BLOCKED_ON_READ)
          (poller/wait-for-read sock.fd)
          (poller/wait-for-write sock.fd)))

    ;; XXX probably need a state variable for this: if a server
    ;;  immediately spawns read/write threads they will both try
    ;;  to negotiate.
    (define (negotiate)
      (let loop ((count 0))
        (when (> count 10)
          (raise (:S2NError 0 0 "negotiation stuck")))
        (let ((status* (halloc int))
              (r0 (s2n/s2n_negotiate conn* status*))
              (r1 (%c-get-int int status*)))
          (printf "[" (int sock.fd) "] s2n_negotiate r0 " (int r0) " r1 " (int r1) "\n")
          (cond ((= r1 S2N_NOT_BLOCKED)
                 r0)
                (else
                 (printf "negotiate: wait\n")
                 (wait-on-block r1)
                 (loop (+ count 1)))))))

    (define (recv0)
      (let ((status* (halloc int))
            (buf* (%c-aref char ibuf.buf ibuf.pos))
            (r0 (s2n/s2n_recv conn* (%c-cast void buf*) ibuf.size status*))
            (r1 (%c-get-int int status*)))
        (printf "[" (int sock.fd) "] s2n_recv r0 " (int r0) " r1 " (int r1) "\n")
        (cond ((= r1 S2N_NOT_BLOCKED)
               (inc! ibuf.end r0)
               (when (= r0 0)
                 (set! closed #t))
               r0)
              (else
               (wait-on-block r1)
               (recv0)))))

    (define (recv)
      (let ((nbytes (recv0))
            (r (buffer/contents ibuf)))
        ;;(printf "nbytes " (int nbytes) " ibuf.pos " (int ibuf.pos) " ibuf.end " (int ibuf.end) "\n")
        (buffer/reset! ibuf)
        r))

    (define (send0)
      (let ((status* (halloc int))
            (buf* (c-aref obuf.buf obuf.pos))
            (r0 (s2n/s2n_send conn* (%c-cast void buf*) (- obuf.end obuf.pos) status*))
            (r1 (c-get-int status*)))
        (printf "[" (int sock.fd) "] s2n_send r0 " (int r0) " r1 " (int r1) "\n")
        ;;(printf "  -- " (int (- obuf.end obuf.pos)) " bytes.\n")
        (cond ((= r1 S2N_NOT_BLOCKED)
               r0)
              (else
               (wait-on-block r1)
               (send0)))))

    ;; 'sendall'
    (define (send data)
      (let ((left (string-length data)))
        (buffer/reset! obuf)
        ;; this is cheating, we should loop here instead
        ;;  of just making the buffer larger.
        (buffer/add! obuf data)
        (while (> left 0)
          (let ((sent (send0)))
            (inc! obuf.pos sent)
            (dec! left sent)
            ))
        ))

    (define (shutdown)
      (if closed
          0
          (let ((status* (halloc int))
                (_ (c-set-int status* 3141))
                (r0 (s2n/s2n_shutdown conn* status*))
                (r1 (c-get-int status*)))
            (printf "[" (int sock.fd) "] s2n_shutdown r0 " (int r0) " r1 " (int r1) "\n")
            (cond ((= r1 S2N_NOT_BLOCKED)
                   r0)
                  (else
                   (wait-on-block r1)
                   (shutdown))))))

    ;; XXX near duplicate of the one in doom/socket.scm
    (define (recv-exact n)
      (if (< n (- ibuf.end ibuf.pos))
          (buffer/get! ibuf n)
          (let ((have (- ibuf.end ibuf.pos))
                (left (- n have))
                (parts (LIST (buffer/get! ibuf have))))
            (while (> left 0)
              (let ((recvd (recv0)))
                (when (= recvd 0)
                  (raise (:Doom/EOF sock)))
                (PUSH parts (buffer/get! ibuf (min left recvd)))
                (dec! left (min left recvd))
                ))
            (string-concat (reverse parts)))))

    (define (accept)
      (let (((sock0 addr) (loop-nb-read sock.fd (sock/accept sock))))
        (:tuple (lambda (isize osize)
                  ;; n.b.: via this lambda, `negotiate` is done inside
                  ;; the client thread, not listening thread.
                  (printf "accept fd = " (int sock0.fd) "\n")
                  (let ((sock1 (s2n/make-sock config S2N_SERVER sock0 isize osize))
                        (nr (sock1.negotiate)))
                    (printf "negotiate -> " (int nr) "\n")
                    sock1))
                addr)))

    ;; pass-through to low socket layer
    (define (listen n)
      ;; XXX zilch the two buffers
      (printf "[" (int sock.fd) "] listen\n")
      (sock/listen sock n))

    (define (bind addr)
      (printf "[" (int sock.fd) "] bind\n")
      (sock/bind sock addr))

    (define (close)
      (printf "[" (int sock.fd) "] close\n")
      (shutdown)
      (sock/close sock)
      )

    ;; body of s2/make-conn
    (sock/set-nonblocking sock)
    (trys2n (s2n/s2n_connection_set_config conn* (config.get)))
    (trys2n (s2n/s2n_connection_set_fd conn* sock.fd))

    {
     negotiate  = negotiate
     send       = send
     recv       = recv
     recv-exact = recv-exact
     accept     = accept
     bind       = bind
     listen     = listen
     close      = close
     }
    ))

;; test
(define (read-file-contents ifile)
  (let loop ((buf (file/read-buffer ifile))
	     (l '()))
    (if (= (string-length buf) 0)
        (string-concat (reverse l))
        (loop (file/read-buffer ifile)
              (list:cons buf l)))))

(define (get-pem-file path)
  (zero-terminate
   (read-file-contents
    (file/open-read path))))

;; to generate a cert/key combo:
;;
;; openssl req -x509 -nodes -newkey rsa:2048 -keyout key.pem -out cert.pem -days 720 -subj '/CN=localhost' -sha256
;;
;; note: as of early 2018, s2n still does not support ecdsa certs.

(define (make-config cert-path skey-path dhparam-path protos)
  (let ((config (s2n-make-config))
        (cert (get-pem-file cert-path))
        (skey (get-pem-file skey-path))
        (dhparam (get-pem-file dhparam-path))
        )
    (config.set-cipher-preferences "default\x00")
    (printf "cipher-preferences\n")
    (config.add-cert-chain-and-key cert skey)
    (printf "add-cert\n")
    (config.add-dhparams dhparam)
    (printf "add-dhparams\n")
    (when (not (null? protos))
      (config.set-protocol-preferences protos)
      (printf "set-protocol-preferences\n"))
    config))

(define (serve config addr0 client-fun)
  (let ((sock (s2n/make-sock config S2N_SERVER (tcp4-sock))))
    (sock.bind addr0)
    (printf "bind ok.\n")
    (sock.listen 5)
    (printf "listen ok.\n")
    (let loop ()
      (printf "accept wait.\n")
      (match (sock.accept) with
        (:tuple sockfun addr1)
        -> (poller/fork (lambda () (client-fun sockfun addr1))))
      (loop))
    ;; notreached - add an exit condition.
    (sock.close)
    ))

