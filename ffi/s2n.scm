;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "lib/map.scm")
(include "lib/net/socket.scm")

(require-ffi 'posix)
(require-ffi 'socket)
(require-ffi 's2n)

(%backend bytecode (update-sizeoff-table))

(define (s2n-init) (s2n/s2n_init))

(define (s2n-cleanup) (s2n/s2n_cleanup))

(define (s2n-strerror n)
  (let ((s* (s2n/s2n_strerror n (%string->cref #f "EN\x00")))
        (slen (posix/strlen s*)))
    (%cref->string #f s* slen)))

(define (get-s2n-error)
  (let ((err (%c-get-int int s2n/s2n_errno))
        (etype (s2n/s2n_error_get_type err)))
    (:S2NError etype err (s2n-strerror err))))

(define (format-s2n-error)
  (match (get-s2n-error) with
    (:S2NError etype err errstr)
    -> (format "type=" (int etype) " code = " (hex err) ": " (string errstr))))

(define (string->c s)
  (%string->cref #f (zero-terminate s)))

(define (trys2n val)
  (if (not (= 0 val))
      (raise (get-s2n-error))
      val))

;; --------------------------------------------------------------------------------
;;                            config object
;; --------------------------------------------------------------------------------

(datatype s2n-config
  (:t {config=(cref (struct s2n_config))}))

(define (s2n-config-maker)

  (define (set-cipher-preferences self version)
    (s2n/s2n_config_set_cipher_preferences self.config (string->c version)))

  (define (add-cert-chain-and-key self chain-pem skey-pem)
    (let ((chain (string->c chain-pem))
          (skey (string->c skey-pem)))
      (s2n/s2n_config_add_cert_chain_and_key self.config chain skey)
      ))

  (define (add-dhparams self dhparams-pem)
    (s2n/s2n_config_add_dhparams self.config (string->c dhparams-pem)))

  (define (get self) self.config)

  (define un (s2n-config:t self) -> self)

  (let ((methods {set-cipher-preferences=set-cipher-preferences
                  add-cert-chain-and-key=add-cert-chain-and-key
                  add-dhparams=add-dhparams un=un get=get}))
    (lambda ()
      (let ((config (s2n/s2n_config_new)))
        (if (cref-null? config)
            (raise (get-s2n-error))
            {o=methods self=(s2n-config:t {config=config})})))
    ))

(define s2n-config (s2n-config-maker))

;; --------------------------------------------------------------------------------
;;                          connection object
;; --------------------------------------------------------------------------------

(datatype s2n-conn
  (:t {conn=(cref (struct s2n_connection))}))

(define (s2n-conn-maker)

  (define (set-config self config)
    (s2n/s2n_connection_set_config self.conn config))

  (define (set-fd self fd)
    (s2n/s2n_connection_set_fd self.conn fd))

  (define (negotiate self)
    (let ((status* (halloc int))
          (r0 (s2n/s2n_negotiate self.conn status*))
          (r1 (%c-get-int int status*)))
      (printf "negotiate: status = " (int r1) "\n")
      r1))

  (define (recv self buf)
    (let ((status* (halloc int))
          (buf* (%c-aref char buf.buf buf.pos))
          (r0 (s2n/s2n_recv self.conn (%c-cast void buf*) buf.size status*))
          (r1 (%c-get-int int status*)))
      ;;(printf "recv: status = " (int r1) " nbytes = " (int r0) "\n")
      (set! buf.end (+ buf.pos r0))
      ;; XXX check r1
      r0))

  (define (send self buf)
    (let ((status* (halloc int))
          (buf* (%c-aref char buf.buf buf.pos))
          (r0 (s2n/s2n_send self.conn (%c-cast void buf*) (- buf.end buf.pos) status*))
          (r1 (%c-get-int int status*)))
      ;;(printf "send: status = " (int r1) " nbytes = " (int r0) "\n")
      ;; XXX check r1
      r0))

  (define (shutdown self)
    (let ((status* (halloc int))
          (r0 (trys2n (s2n/s2n_shutdown self.conn status*)))
          (r1 (%c-get-int int status*)))
      r0))

  (define un (s2n-conn:t self) -> self)

  (let ((methods {set-config=set-config
                  set-fd=set-fd
                  negotiate=negotiate
                  send=send recv=recv shutdown=shutdown
                  un=un}))
    (lambda (mode)
      (let ((conn (s2n/s2n_connection_new mode)))
        (if (cref-null? conn)
            (raise (get-s2n-error))
            {o=methods self=(s2n-conn:t {conn=conn})})))
    ))

(define s2n-conn (s2n-conn-maker))

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

(define (serve config addr0 sock0 client-fun)
  (printf "fd0 = " (int sock0.fd) "\n")
  (sock/bind sock0 addr0)
  (printf "bind ok.\n")
  (sock/listen sock0 5)
  (printf "listen ok.\n")
  (let loop ()
    (match (sock/accept sock0) with
      (:tuple sock1 addr1)
      -> (client-fun config sock1 addr1))
    (loop))
  (sock/close sock0)
  )

(define (make-config)
  (let ((config (s2n-config))
        (cert (get-pem-file "doom/tls/server.crt"))
        (skey (get-pem-file "doom/tls/server.raw.key"))
        (dhparam (get-pem-file "doom/tls/dhparam.pem")))
    (trys2n (config::set-cipher-preferences "default\x00"))
    (printf "cipher-preferences\n")
    (trys2n (config::add-cert-chain-and-key cert skey))
    (printf "add-cert\n")
    (trys2n (config::add-dhparams dhparam))
    (printf "add-dhparams\n")
    config))

(define the-config (make-config))

(define (echo-client config sock addr)
  (let ((conn (s2n-conn S2N_SERVER)))
    (trys2n (conn::set-config (config::get)))
    (printf "conn::set-config\n")
    (conn::set-fd sock.fd)
     (let ((buf (buffer/make 4096)))
       (try
        (begin
          (conn::negotiate)
          (printf "negotiated.\n")
          (let loop ()
            (conn::recv buf)
            (when (> buf.end 0)
              (conn::send buf)
              (buffer/reset! buf)
              (loop)
              ))
          (conn::shutdown)
          )
        except
        (:S2NError etype err errstr)
        -> (if (= etype S2N_ERR_T_CLOSED)
               0
               (raise (:S2NError etype err errstr)))
        ))
     (sock/close sock)
     ))

(define (go)
  (printf "s2n_init() => " (int (s2n-init)) "\n")
  (printn (get-s2n-error))
  (let ((config (make-config)))
    (printf "serving...\n")
    (serve config (address/make4 "127.0.0.1" 9002) (tcp4-sock) echo-client))
  )

(go)

