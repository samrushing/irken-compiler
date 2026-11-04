;; -*- Mode: Irken -*-

(require "doom/ssh/ssh.scm")
(require "doom/ssh/test/linedit.scm")

(require "glulx/vm.scm")

(define server-host-pkey
  (hexcat "38e9939f5fa259b474172924db12efddb2e78e9e1363587440b03d355123ea29"))

(define server-host-skey
  (hexcat "4712153bb4b3d22da8921160b10691282de9b26a4a97598f3c735d126de28b5d"
          "38e9939f5fa259b474172924db12efddb2e78e9e1363587440b03d355123ea29"))

(define pwdauth
  (make-password-authenticator
   (tree/make
    string-cmp
    ("foo" "xyzzy")
    )))

(define pkeyauth
  (make-publickey-authenticator
   (tree/make
    string-cmp
    ("foo"
     (tree/make
      string-cmp
      ("ssh-ed25519"
       (hexcat "0000000b7373682d6564323535313900000020b7b9646f92"
               "8212b61456dc45aca79e2347e1a94c16811c00f4a07fc3313ee9e2"))
      ))
    ("mburr"
     (tree/make
      string-cmp
      ("ssh-ed25519"
       (hexcat "0000000b7373682d656432353531390000002084b2d0acc4"
               "985b7aa88ed734a86188aaf5fc2b96d1ca2e3f5da64e77039dad07"))
      ))
    )))

(define (serve game-path ip port)

  (define (session-handler chan)

    (let ((read-line-fun (line-edit chan)))

    (define (write-string s)
      (chan.write
       (string-join
        (string-split s #\newline)
        "\r\n")))

    (define (read-line)
      (let ((mline (read-line-fun)))
        (chan.write "\r\n")
        mline))

    ;; (read-line)
    (run-game game-path read-line write-string)
    (chan.close)
    ))

  (let ((sock (doom/make (tcp4-sock)))
        (addr (address/make4 ip port)))
    (sock.bind addr)
    (sock.listen 5)
    (printf "starting server...\n")
    (let loop ()
      (printf "accept wait...\n")
      (let (((sockfun addr) (sock.accept)))
        (printf "client: addr " (address/unparse addr) "\n")
        (poller/fork (lambda () (ssh-conn-loop sockfun (list pwdauth pkeyauth) session-handler)))
        (loop)))))

(serve sys.argv[1] "0.0.0.0" 8222)
(poller/wait-and-schedule)
