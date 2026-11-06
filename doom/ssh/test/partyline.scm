;; -*- Mode: Irken -*-

;; party line: a tiny chat room over ssh.
;;
;; everyone who connects is in the same room.  lines you type go to
;;  everyone else; messages from others appear above the line you're
;;  typing, which is redrawn so it doesn't get clobbered.
;;
;; usage:  doom/ssh/test/partyline [port]          (default port 8222)
;;         ssh -p 8222 foo@host                    (password 'bar')

(require "lib/posix.scm")
(require "doom/ssh/ssh.scm")
(require "doom/ssh/test/linedit.scm")
(require "lib/csi.scm")

(define server-host-pkey
  (hex->string "38e9939f5fa259b474172924db12efddb2e78e9e1363587440b03d355123ea29"))

(define server-host-skey
  (hex->string "4712153bb4b3d22da8921160b10691282de9b26a4a97598f3c735d126de28b5d38e9939f5fa259b474172924db12efddb2e78e9e1363587440b03d355123ea29"))

(define pwdauth
  (make-password-authenticator
   (tree/make
    string-cmp
    ("foo" "bar")
    ("bar" "foo")
    )))

(define pkeyauth
  (make-publickey-authenticator
   (tree/make
    string-cmp
    ("foo"
     (tree/make
      string-cmp
      ("ssh-ed25519" (hex->string "0000000b7373682d6564323535313900000020b7b9646f928212b61456dc45aca79e2347e1a94c16811c00f4a07fc3313ee9e2"))
      ))
    ("mburr"
     (tree/make
      string-cmp
      ("ssh-ed25519" (hex->string "0000000b7373682d656432353531390000002084b2d0acc4985b7aa88ed734a86188aaf5fc2b96d1ca2e3f5da64e77039dad07"))
      ))
    )))

;; ------------------------------------------------------------
;; the room
;; ------------------------------------------------------------

;; each member is {id nick editor}, where editor is from make-line-editor.
;;  (only one thread runs at a time, so plain set! on the list is safe.)

(define members (list:nil))

(define next-member-id
  (let ((counter 0))
    (lambda ()
      (inc! counter)
      counter)))

(define (room/add! m)
  (set! members (list:cons m members)))

(define (room/remove! id)
  (set! members (filter (lambda (m) (not (= m.id id))) members)))

(define (room/nicks)
  (map (lambda (m) m.nick) (reverse members)))

(define (room/nick-taken? nick)
  (let loop ((ms members))
    (match ms with
      ()        -> #f
      (m . tl)  -> (if (string=? m.nick nick) #t (loop tl)))))

;; send to everyone (including the sender, so all transcripts match).
;;  `members` is an immutable list, so iterating a snapshot is fine even
;;  if a write yields and someone joins or leaves meanwhile.
(define (room/broadcast msg)
  (for-list m members
    (m.editor.interject msg)))

(define (room/broadcast-except id msg)
  (for-list m members
    (when (not (= m.id id))
      (m.editor.interject msg))))

;; ------------------------------------------------------------
;; formatting
;; ------------------------------------------------------------

;; wrap `text` in the given SGR attributes, then reset.
(define (styled attrs text)
  (format (csi (csi:sgr attrs)) text (csi (csi:sgr (list SGR-reset)))))

;; a stable color per nick, so conversations are easier to follow.
(define nick-colors
  (list->vector (list SGR-red SGR-green SGR-yellow SGR-blue SGR-magenta SGR-cyan)))

(define (color-nick nick)
  (let ((h 0))
    (for-string ch nick
      (set! h (+ (* h 31) (char->int ch))))
    (styled (list SGR-bold (sgr-fg nick-colors[(mod h (vector-length nick-colors))])) nick)))

(define (notice text)
  (styled (list SGR-dim) (format "*** " text)))

(define (say-line nick text)
  (format "<" (color-nick nick) "> " text))

(define (emote-line nick text)
  (format "* " (color-nick nick) " " text))

(define help-text
  (list
   "commands:"
   "  /who          list who's here"
   "  /nick <name>  change your nick"
   "  /me <action>  emote, e.g. /me waves"
   "  /quit         leave (or ctrl-d)"
   "  /help         this message"))

;; split "/cmd rest of line" into (:tuple "cmd" "rest of line")
(define (split-command line)
  (let ((sp (string-find " " line))
        (n (string-length line)))
    (if (= sp -1)
        (:tuple (substring line 1 n) "")
        (:tuple (substring line 1 sp) (substring line (+ sp 1) n)))))

(define (valid-nick? nick)
  (let ((n (string-length nick))
        (ok #t))
    (for-string ch nick
      (let ((c (char->int ch)))
        (when (or (<= c #x20) (> c #x7e))
          (set! ok #f))))
    (and ok (> n 0) (<= n 16))))

;; ------------------------------------------------------------
;; a session
;; ------------------------------------------------------------

(define (session-handler chan)

  (let ((ed (make-line-editor chan))
        (id (next-member-id))
        (nick ""))

    (define (tell msg)
      (ed.interject msg))

    ;; ask for a nick until we get a usable one.
    (define (choose-nick)
      (match (ed.prompt-read "nick: ") with
        (maybe:no) -> (maybe:no)
        (maybe:yes name)
        -> (begin
             (chan.write "\r\n")
             (cond ((not (valid-nick? name))
                    (tell "nicks are 1-16 printable characters, no spaces.")
                    (choose-nick))
                   ((room/nick-taken? name)
                    (tell (format "'" name "' is taken, try another."))
                    (choose-nick))
                   (else (maybe:yes name))))))

    (define (do-who)
      (tell (notice (format (int (length members)) " here: "
                            (string-join (room/nicks) ", ")))))

    (define (do-nick new)
      (cond ((not (valid-nick? new))
             (tell (notice "usage: /nick <name>  (1-16 printable characters, no spaces)")))
            ((room/nick-taken? new)
             (tell (notice (format "'" new "' is taken."))))
            (else
             (let ((old nick))
               (set! nick new)
               ;; members holds a copy of the record, so swap it out.
               (room/remove! id)
               (room/add! {id=id nick=new editor=ed})
               (room/broadcast (notice (format old " is now known as " (color-nick new))))))))

    (define (do-command line)
      (match (split-command line) with
        (:tuple "quit" _)   -> #f
        (:tuple "who" _)    -> (begin (do-who) #t)
        (:tuple "help" _)   -> (begin (for-list l help-text (tell l)) #t)
        (:tuple "nick" new) -> (begin (do-nick new) #t)
        (:tuple "me" act)   -> (begin (room/broadcast (emote-line nick act)) #t)
        (:tuple cmd _)      -> (begin (tell (notice (format "unknown command /" cmd ", try /help"))) #t)
        ))

    ;; returns #f when the user wants to leave.
    (define (handle-line line)
      ;; replace the echoed input with the transcript line.
      (chan.write (format "\r" (csi (csi:el 2))))
      (cond ((= (string-length line) 0) #t)
            ((starts-with line "/") (do-command line))
            (else (room/broadcast (say-line nick line)) #t)))

    (define (chat-loop)
      (match (ed.prompt-read "> ") with
        (maybe:no) -> #u
        (maybe:yes line) -> (when (handle-line line) (chat-loop))))

    (chan.write (format "\r\n" (styled (list SGR-bold) "welcome to the irken party line.")
                        "  type /help for commands.\r\n\r\n"))
    (try
     (match (choose-nick) with
       (maybe:no) -> #u
       (maybe:yes name)
       -> (begin
            (set! nick name)
            (room/broadcast (notice (format (color-nick nick) " has joined.")))
            (room/add! {id=id nick=nick editor=ed})
            (do-who)
            (chat-loop)))
     except
     (:LineEdit/EOF) -> #u
     )
    ;; leaving, whether by /quit, ctrl-d, or a dropped connection.
    (when (> (string-length nick) 0)
      (room/remove! id)
      (room/broadcast (notice (format (color-nick nick) " has left."))))
    (chan.write "\r\nbye.\r\n")
    (chan.close)
    (printf "session " (int id) " (" nick ") done.\n")
    ))

(define (serve ip port)
  (let ((sock (doom/make (tcp4-sock)))
        (addr (address/make4 ip port)))
    (sock.bind addr)
    (sock.listen 5)
    (printf "party line listening on port " (int port) "\n")
    (let loop ()
      (let (((sockfun addr) (sock.accept)))
        (printf "client: addr " (address/unparse addr) "\n")
        (poller/fork (lambda () (ssh-conn-loop sockfun (list pwdauth pkeyauth) session-handler)))
        (loop)))))

;; a write to a vanished client must not take down the whole room.
(signal/ignore SIGPIPE)

(serve "0.0.0.0" (if (> sys.argc 1) (string->int sys.argv[1]) 8222))
(poller/wait-and-schedule)
