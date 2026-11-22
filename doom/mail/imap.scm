;; -*- Mode: Irken -*-

;; IMAP client (rfc3501, IMAP4rev1).
;;
;; supported: STARTTLS, or TLS from the start (port 993, rfc8314);
;;  AUTHENTICATE PLAIN (with SASL-IR) or LOGIN; LIST, SELECT/EXAMINE,
;;  STATUS, UID SEARCH, UID FETCH (streaming, see fetch), UID STORE,
;;  UID COPY, UID MOVE (rfc6851), UID EXPUNGE (rfc4315), APPEND, CREATE,
;;  DELETE, RENAME, NOOP, LOGOUT; literals both ways (LITERAL+, rfc7888,
;;  when the server has it).
;; not yet: IDLE, modified UTF-7 mailbox names (they're passed as they
;;  are), COMPRESS, CONDSTORE/QRESYNC.
;;
;; values from the server are imapv: atoms, numbers, strings (quoted
;;  or literal), NIL, and parenthesized lists.
;;
;; errors:
;;   (:IMAP/No what text)     the server said NO to <what>
;;   (:IMAP/Bad what text)    the server said BAD to <what>
;;   (:IMAP/Bye text)         the server is closing the connection
;;   (:IMAP/Protocol why)     the server's responses make no sense
;;   (:IMAP/BadArgument s)    <s> can't go in a command
;;   (:IMAP/NoTLS)            STARTTLS was wanted, but isn't offered
;;   (:IMAP/Busy why)         this thread has a fetch stream open
;;   (:Reader/Closed what)    the server hung up

(require "lib/codecs/base64.scm")
(require "doom/doom.scm")
(require "doom/reader.scm")
(require "doom/net.scm")
(require "doom/tls/client.scm")

(datatype imap-security
  (:none)      ;; plain text
  (:starttls)  ;; plain text, upgraded to TLS before logging in (port 143)
  (:implicit)  ;; TLS from the start (port 993)
  )

(datatype imapv
  (:atom string)       ;; OK, FLAGS, \Seen, BODY[HEADER.FIELDS (FROM)]
  (:num int)
  (:str string)        ;; quoted or literal
  (:nil)
  (:list (list imapv))
  )

(define imapv-repr
  (imapv:atom s) -> s
  (imapv:num n)  -> (int->string n)
  (imapv:str s)  -> (repr-string s)
  (imapv:nil)    -> "NIL"
  (imapv:list l) -> (format "(" (join imapv-repr " " l) ")")
  )

;; one response from the server.
(datatype imap-resp
  ;; tag ("*" if untagged), OK/NO/BAD/BYE/PREAUTH, the response code
  ;;  (the part in [brackets], as values), and the text.
  (:status string string (list imapv) string)
  ;; any other untagged response: "* 23 EXISTS" -> (num 23) (atom EXISTS)
  (:data (list imapv))
  ;; "+ text": the server waits for a literal, or for SASL data.
  (:cont string)
  )

;; --- parsing ---

(define (imap/digit? ch)
  (let ((n (char->int ch))) (and (>= n 48) (<= n 57))))

(define (imap/all-digits? s)
  (and (> (string-length s) 0)
       (let loop ((i 0))
         (cond ((= i (string-length s)) #t)
               ((imap/digit? (string-ref s i)) (loop (+ i 1)))
               (else #f)))))

;; the values in <s>, from <pos> to the end.  <s> is a response as it
;;  came, literals and all: {n} CRLF followed by n bytes.
(define (imap/parse s pos)
  (let ((pos pos)
        (n (string-length s)))

    (define (peek) (if (< pos n) (string-ref s pos) #\nul))
    (define (eof?) (>= pos n))

    (define (skip-spaces)
      (while (and (not (eof?)) (eq? (peek) #\space))
        (inc! pos)))

    (define (bad why)
      (raise (:IMAP/Protocol (format why " at " (int pos) " in " (string (substring s 0 (min n 200)))))))

    (define (quoted)
      (inc! pos) ;; the "
      (let ((parts (list:nil)))
        (let loop ()
          (when (eof?) (bad "unterminated string"))
          (let ((ch (peek)))
            (inc! pos)
            (cond ((eq? ch #\") #u)
                  ((eq? ch #\\)
                   (when (eof?) (bad "unterminated string"))
                   (push! parts (char->string (peek)))
                   (inc! pos)
                   (loop))
                  (else (push! parts (char->string ch)) (loop)))))
        (imapv:str (string-concat (reverse parts)))))

    ;; {123} CRLF <123 bytes>, or {123+} (from LITERAL+), or ~{123} (literal8)
    (define (literal)
      (when (eq? (peek) #\~) (inc! pos))
      (inc! pos) ;; the {
      (let ((start pos))
        (while (and (not (eof?)) (imap/digit? (peek))) (inc! pos))
        (let ((len (string->int (substring s start pos))))
          (when (eq? (peek) #\+) (inc! pos))
          (when (not (eq? (peek) #\})) (bad "bad literal"))
          (inc! pos)
          (when (not (and (< (+ pos 1) n) (eq? (string-ref s pos) #\return) (eq? (string-ref s (+ pos 1)) #\newline)))
            (bad "no CRLF after literal"))
          (inc! pos 2)
          (when (> (+ pos len) n) (bad "short literal"))
          (inc! pos len)
          (imapv:str (substring s (- pos len) pos)))))

    ;; an atom; a [section] in it is taken whole, spaces, parens and all:
    ;;  BODY[HEADER.FIELDS (FROM TO)]<0>
    (define (atom)
      (let ((start pos))
        (let loop ()
          (when (not (eof?))
            (let ((ch (peek)))
              (cond ((eq? ch #\[)
                     (while (and (not (eof?)) (not (eq? (peek) #\]))) (inc! pos))
                     (when (eof?) (bad "unterminated ["))
                     (inc! pos)
                     (loop))
                    ((or (eq? ch #\space) (eq? ch #\() (eq? ch #\)) (eq? ch #\") (eq? ch #\{)
                         (eq? ch #\return) (eq? ch #\newline))
                     #u)
                    (else (inc! pos) (loop))))))
        (when (= start pos) (bad "expected a value"))
        (let ((a (substring s start pos)))
          (cond ((string=? (upcase a) "NIL") (imapv:nil))
                ((and (imap/all-digits? a) (< (string-length a) 19)) (imapv:num (string->int a)))
                (else (imapv:atom a))))))

    (define (value)
      (skip-spaces)
      (match (peek) with
        #\( -> (begin (inc! pos) (imapv:list (values-until #\))))
        #\" -> (quoted)
        #\{ -> (literal)
        #\~ -> (if (and (< (+ pos 1) n) (eq? (string-ref s (+ pos 1)) #\{)) (literal) (atom))
        _   -> (atom)))

    ;; values up to <close> (or the end, if close is #\nul)
    (define (values-until close)
      (let loop ((acc (list:nil)))
        (skip-spaces)
        (cond ((eof?)
               (if (eq? close #\nul) (reverse acc) (bad "unterminated list")))
              ((eq? (peek) close)
               (inc! pos)
               (reverse acc))
              ((and (eq? close #\nul) (eq? (peek) #\)))
               (bad "unbalanced )"))
              (else (loop (list:cons (value) acc))))))

    (values-until #\nul)))

;; "* OK [UIDNEXT 4] Predicted next UID" (after the tag) -> code and text
(define (imap/parse-status-rest s pos)
  (let ((n (string-length s))
        (pos (if (and (< pos n) (eq? (string-ref s pos) #\space)) (+ pos 1) pos)))
    (if (and (< pos n) (eq? (string-ref s pos) #\[))
        (let ((close (string-find-from "]" s pos)))
          (if (= close -1)
              (:tuple (list:nil) (substring s pos n))
              (:tuple (imap/parse (substring s (+ pos 1) close) 0)
                      (string-trim-left (substring s (+ close 1) n)))))
        (:tuple (list:nil) (substring s pos n)))))

;; a whole response, as text: one line, plus any literal (and the rest
;;  of the line after it).
(define (imap/read-raw r)
  (let loop ((parts (list:nil)))
    (let ((line (r.read-line))
          (n (string-length line)))
      ;; does it end with a literal's {123}?
      (let ((open (if (and (> n 2) (eq? (string-ref line (- n 1)) #\}))
                      (let back ((i (- n 2)))
                        (cond ((< i 0) -1)
                              ((imap/digit? (string-ref line i)) (back (- i 1)))
                              ((eq? (string-ref line i) #\{) (if (< i (- n 2)) i -1))
                              (else -1)))
                      -1)))
        (if (= open -1)
            (string-concat (reverse (list:cons line parts)))
            (let ((len (string->int (substring line (+ open 1) (- n 1)))))
              (loop (list:cons (r.read-n len) (list:cons "\r\n" (list:cons line parts))))))))))

(define (imap/parse-response s)
  (let ((sp (string-find " " s))
        (tag (if (= sp -1) s (substring s 0 sp))))
    (cond ((string=? tag "+")
           (imap/cont (if (= sp -1) "" (substring s (+ sp 1) (string-length s)))))
          ((= sp -1)
           (raise (:IMAP/Protocol (format "bad response: " (string s)))))
          (else
           (let ((sp2 (string-find-from " " s (+ sp 1)))
                 (word (upcase (substring s (+ sp 1) (if (= sp2 -1) (string-length s) sp2)))))
             (if (member? word '("OK" "NO" "BAD" "BYE" "PREAUTH") string=?)
                 (match (imap/parse-status-rest s (if (= sp2 -1) (string-length s) sp2)) with
                   (:tuple code text) -> (imap-resp:status tag word code text))
                 (if (string=? tag "*")
                     (imap-resp:data (imap/parse s (+ sp 1)))
                     (raise (:IMAP/Protocol (format "bad tagged response: " (string s)))))))))))

(define (imap/cont text) (imap-resp:cont text))

;; --- encoding ---

;; how to send <s>: an atom if we can, a quoted string if we can,
;;  else a literal.  (:tuple #f text) or (:tuple #t bytes).
(define (imap/astring s)
  (define (atom-char? ch)
    (let ((n (char->int ch)))
      (and (> n 32) (< n 127)
           (not (member-eq? ch '(#\( #\) #\{ #\space #\% #\* #\" #\\ #\]))))))
  (define (quotable? ch)
    (let ((n (char->int ch)))
      (and (> n 0) (< n 128) (not (eq? ch #\return)) (not (eq? ch #\newline)))))
  (define (every? p)
    (let loop ((i 0))
      (cond ((= i (string-length s)) #t)
            ((p (string-ref s i)) (loop (+ i 1)))
            (else #f))))
  (cond ((and (> (string-length s) 0) (every? atom-char?)
              (not (string=? (upcase s) "NIL")))
         (:tuple #f s))
        ((every? quotable?)
         (let ((parts (list "\"")))
           (for-string ch s
             (when (or (eq? ch #\") (eq? ch #\\)) (push! parts "\\"))
             (push! parts (char->string ch)))
           (push! parts "\"")
           (:tuple #f (string-concat (reverse parts)))))
        (else (:tuple #t s))))

;; a set of UIDs, "1:3,7,9:12", from a list of numbers.
(define (imap/uid-set uids)
  (let ((sorted (sort < uids))
        (parts (list:nil)))
    (define (range lo hi)
      (push! parts (if (= lo hi) (int->string lo) (format (int lo) ":" (int hi)))))
    (match sorted with
      () -> (raise (:IMAP/BadArgument "an empty UID set"))
      (first . rest)
      -> (let loop ((lo first) (hi first) (l rest))
           (match l with
             () -> (range lo hi)
             (x . tl) -> (cond ((= x hi) (loop lo hi tl))
                               ((= x (+ hi 1)) (loop lo x tl))
                               (else (range lo hi) (loop x x tl)))))
      )
    (string-join (reverse parts) ",")))

;; --- results ---

;; a FETCH item, by name ("FLAGS", "UID", "BODY[]", ...).
(define (imap/item items name)
  (let loop ((l items))
    (match l with
      () -> (maybe:no)
      ((:tuple k v) . tl) -> (if (string=? k name) (maybe:yes v) (loop tl))
      )))

;; the text of a FETCH item that's a string (e.g. BODY[]), or "".
(define (imap/item-string items name)
  (match (imap/item items name) with
    (maybe:yes (imapv:str s)) -> s
    _ -> ""))

;; a list of atoms (e.g. FLAGS) as strings.
(define (imap/atoms v)
  (match v with
    (imapv:list l)
    -> (map (lambda (x) (match x with (imapv:atom a) -> a (imapv:str s) -> s _ -> (imapv-repr x))) l)
    _ -> (list:nil)))

;; (atom NAME) value (atom NAME) value ... -> (:tuple NAME value) ...
(define (imap/pairs l)
  (match l with
    () -> (list:nil)
    ((imapv:atom k) v . tl) -> (list:cons (:tuple (upcase k) v) (imap/pairs tl))
    _ -> (raise (:IMAP/Protocol "bad name/value list"))
    ))

;; --- sessions ---

;; a session over <conn> (a {send recv close}; see doom/stream), which
;;  must be at the start: the server's greeting not yet read.  <tls?>
;;  says whether conn is already TLS.
;;
;; returns a record of operations; see the end of this function.
(define (imap/client conn tls?)

  (let ((conn conn)
        (r (reader/make conn))
        (tls? tls?)
        (caps (list:nil))
        (tag-counter 0)
        (events (list:nil))      ;; untagged responses nobody asked for
        (mutex (mutex/make))
        (stream-thread -1)
        (greeting ""))

    (define (send s) (conn.send s))

    (define (read-response)
      (imap/parse-response (imap/read-raw r)))

    (define (has-cap? name)
      (member? (upcase name) caps string=?))

    (define (set-caps l)
      (set! caps (map (lambda (v) (upcase (match v with (imapv:atom a) -> a _ -> (imapv-repr v)))) l)))

    ;; a response code that holds capabilities.
    (define (check-code code)
      (match code with
        ((imapv:atom "CAPABILITY") . l) -> (set-caps l)
        _ -> #u))

    (define (check-busy)
      (when (= stream-thread *thread-id*)
        (raise (:IMAP/Busy "a fetch stream is still open on this connection: close it first (for-closing does)"))))

    ;; --- commands ---

    ;; send a command: parts are (:tuple literal? text).  a literal is
    ;;  sent as {n+} if the server has LITERAL+; else as {n}, waiting
    ;;  for the server's +.
    (define (send-command tag parts)
      (let ((line (list tag)))
        (for-list part parts
          (match part with
            (:tuple #f text)
            -> (begin
                 (for-string ch text
                   (when (or (eq? ch #\return) (eq? ch #\newline) (= 0 (char->int ch)))
                     (raise (:IMAP/BadArgument text))))
                 (push! line " ")
                 (push! line text))
            (:tuple #t bytes)
            -> (let ((plus? (has-cap? "LITERAL+")))
                 (push! line (format " {" (int (string-length bytes)) (if plus? "+" "") "}\r\n"))
                 (if plus?
                     (push! line bytes)
                     (begin
                       (send (string-concat (reverse line)))
                       (set! line (list:nil))
                       (let loop ()
                         (match (read-response) with
                           (imap-resp:cont _) -> #u
                           (imap-resp:status t cond _ text)
                           -> (if (string=? t tag)
                                  (raise (if (string=? cond "NO") (:IMAP/No "literal" text) (:IMAP/Bad "literal" text)))
                                  (begin (note-untagged (imap-resp:status t cond (list:nil) text)) (loop)))
                           other -> (begin (note-untagged other) (loop))))
                       (push! line bytes))))))
        (push! line "\r\n")
        (send (string-concat (reverse line)))))

    (define (note-untagged resp)
      (match resp with
        (imap-resp:status _ "BYE" _ text) -> (raise (:IMAP/Bye text))
        (imap-resp:status _ _ code _) -> (begin (check-code code) (push! events resp))
        (imap-resp:data ((imapv:atom "CAPABILITY") . l)) -> (set-caps l)
        _ -> (push! events resp)))

    (define (next-tag)
      (inc! tag-counter)
      (format "a" (int tag-counter)))

    ;; run a command, returning {code text untagged}: the tagged OK's
    ;;  response code and text, and the untagged responses before it
    ;;  that <wanted?> accepts (the rest go to events).
    (define (run what parts wanted?)
      (let ((tag (next-tag))
            (mine (list:nil)))
        (send-command tag parts)
        (let loop ()
          (match (read-response) with
            (imap-resp:status t cond code text)
            -> (if (string=? t tag)
                   (begin
                     (check-code code)
                     (match cond with
                       "OK" -> {code=code text=text untagged=(reverse mine)}
                       "NO" -> (raise (:IMAP/No what text))
                       _    -> (raise (:IMAP/Bad what text))))
                   (let ((resp (imap-resp:status t cond code text)))
                     (if (wanted? resp)
                         (begin (check-code code) (push! mine resp))
                         (note-untagged resp))
                     (loop)))
            (imap-resp:cont _) -> (raise (:IMAP/Protocol (format "unexpected + during " what)))
            resp -> (begin
                      (if (wanted? resp) (push! mine resp) (note-untagged resp))
                      (loop))))))

    (define (text s) (:tuple #f s))
    (define (astring s) (imap/astring s))
    (define (none resp) #f)

    ;; --- locking ---

    (define (locked what parts wanted?)
      (check-busy)
      (mutex.acquire)
      (try
       (run what parts wanted?)
       finally
       (mutex.release)))

    ;; --- operations ---

    (define (capability)
      (locked "CAPABILITY" (list (text "CAPABILITY")) none)
      caps)

    (define (starttls upgrade)
      (when (not (has-cap? "STARTTLS"))
        (raise (:IMAP/NoTLS)))
      (locked "STARTTLS" (list (text "STARTTLS")) none)
      ;; anything already buffered came before the handshake (cf. CVE-2021-33515).
      (when (> (r.ready) 0)
        (raise (:IMAP/Protocol "data received after the STARTTLS reply")))
      (set! conn (upgrade))
      (set! r (reader/make conn))
      (set! tls? #t)
      ;; what we learned before TLS can't be trusted.
      (capability)
      #u)

    (define (login user password)
      (let ((res (if (and (has-cap? "AUTH=PLAIN") (has-cap? "SASL-IR"))
                     (locked "AUTHENTICATE" (list (text "AUTHENTICATE PLAIN")
                                                  (text (b64-encode (format "\x00" user "\x00" password))))
                             none)
                     (begin
                       (when (has-cap? "LOGINDISABLED")
                         (raise (:IMAP/No "LOGIN" "the server won't take a password now (LOGINDISABLED)")))
                       (locked "LOGIN" (list (text "LOGIN") (astring user) (astring password)) none)))))
        ;; the capabilities change after login: if the server didn't
        ;;  say what they are now, ask.
        (match res.code with
          ((imapv:atom "CAPABILITY") . _) -> #u
          _ -> (begin (capability) #u))))

    (define (data-named? name)
      (lambda (resp)
        (match resp with
          (imap-resp:data ((imapv:atom n) . _)) -> (string=? (upcase n) name)
          _ -> #f)))

    ;; LIST: (list {flags delimiter name})
    (define (list-mailboxes reference pattern)
      (let ((res (locked "LIST" (list (text "LIST") (astring reference) (astring pattern)) (data-named? "LIST"))))
        (map (lambda (resp)
               (match resp with
                 (imap-resp:data (_ flags delim name))
                 -> {flags=(imap/atoms flags)
                     delimiter=(match delim with (imapv:str d) -> d _ -> "")
                     name=(match name with (imapv:str n) -> n (imapv:atom n) -> n (imapv:num n) -> (int->string n) _ -> "")}
                 _ -> (raise (:IMAP/Protocol "bad LIST response"))))
             res.untagged)))

    ;; SELECT or EXAMINE: {exists flags uidvalidity uidnext read-only}
    (define (select* cmd mailbox)
      (let ((res (locked cmd (list (text cmd) (astring mailbox))
                         (lambda (resp) (match resp with
                                          (imap-resp:status "*" _ _ _) -> #t
                                          (imap-resp:data _) -> #t
                                          _ -> #f))))
            (exists 0)
            (flags (list:nil))
            (uidvalidity 0)
            (uidnext 0))
        (for-list resp res.untagged
          (match resp with
            (imap-resp:data ((imapv:num n) (imapv:atom "EXISTS"))) -> (set! exists n)
            (imap-resp:data ((imapv:atom "FLAGS") l)) -> (set! flags (imap/atoms l))
            (imap-resp:status _ _ ((imapv:atom "UIDVALIDITY") (imapv:num n)) _) -> (set! uidvalidity n)
            (imap-resp:status _ _ ((imapv:atom "UIDNEXT") (imapv:num n)) _) -> (set! uidnext n)
            _ -> #u))
        {exists=exists flags=flags uidvalidity=uidvalidity uidnext=uidnext
         read-only=(match res.code with ((imapv:atom "READ-ONLY")) -> #t _ -> #f)}))

    (define (select mailbox) (select* "SELECT" mailbox))
    (define (examine mailbox) (select* "EXAMINE" mailbox))

    ;; STATUS mailbox (MESSAGES UNSEEN ...): (list (:tuple ITEM n))
    (define (status mailbox items)
      (let ((res (locked "STATUS" (list (text "STATUS") (astring mailbox)
                                        (text (format "(" (join " " items) ")")))
                         (data-named? "STATUS"))))
        (match res.untagged with
          ((imap-resp:data (_ _ (imapv:list l))))
          -> (map (lambda (p) (match p with
                                (:tuple k (imapv:num n)) -> (:tuple k n)
                                (:tuple k _) -> (:tuple k -1)))
                  (imap/pairs l))
          _ -> (raise (:IMAP/Protocol "no STATUS response")))))

    ;; UID SEARCH <criteria>: the UIDs.  <criteria> goes as it is (see
    ;;  imap/astring for quoting strings in it).
    (define (search criteria)
      (let ((res (locked "SEARCH" (list (text "UID SEARCH") (text criteria)) (data-named? "SEARCH"))))
        (let ((uids (list:nil)))
          (for-list resp res.untagged
            (match resp with
              (imap-resp:data (_ . nums))
              -> (for-list v nums
                   (match v with
                     (imapv:num n) -> (push! uids n)
                     _ -> #u))
              _ -> #u))
          (reverse uids))))

    ;; UID STORE uids +FLAGS.SILENT (\Seen): <how> is "+FLAGS", "-FLAGS" or "FLAGS".
    (define (store uids how flags)
      (locked "STORE" (list (text "UID STORE") (text (imap/uid-set uids))
                            (text (format how ".SILENT (" (join " " flags) ")")))
              none)
      #u)

    (define (copy uids mailbox)
      (locked "COPY" (list (text "UID COPY") (text (imap/uid-set uids)) (astring mailbox)) none)
      #u)

    ;; MOVE if the server has it, else COPY, \Deleted, and UID EXPUNGE.
    (define (move uids mailbox)
      (if (has-cap? "MOVE")
          (begin
            (locked "MOVE" (list (text "UID MOVE") (text (imap/uid-set uids)) (astring mailbox)) none)
            #u)
          (begin
            (copy uids mailbox)
            (store uids "+FLAGS" (list "\\Deleted"))
            (expunge uids))))

    ;; UID EXPUNGE <uids> (with UIDPLUS), else EXPUNGE (all \Deleted!).
    (define (expunge uids)
      (if (and (has-cap? "UIDPLUS") (not (null? uids)))
          (locked "EXPUNGE" (list (text "UID EXPUNGE") (text (imap/uid-set uids))) none)
          (locked "EXPUNGE" (list (text "EXPUNGE")) none))
      #u)

    ;; APPEND a message (rfc5322, with CRLFs) to <mailbox>.  returns its
    ;;  UID, or 0 if the server doesn't say (no UIDPLUS).
    (define (append mailbox flags message)
      (let ((res (locked "APPEND" (list (text "APPEND") (astring mailbox)
                                        (text (format "(" (join " " flags) ")"))
                                        (:tuple #t message))
                         none)))
        (match res.code with
          ((imapv:atom "APPENDUID") _ (imapv:num uid)) -> uid
          _ -> 0)))

    (define (create mailbox) (locked "CREATE" (list (text "CREATE") (astring mailbox)) none) #u)
    (define (delete mailbox) (locked "DELETE" (list (text "DELETE") (astring mailbox)) none) #u)
    (define (rename from to) (locked "RENAME" (list (text "RENAME") (astring from) (astring to)) none) #u)
    (define (noop) (locked "NOOP" (list (text "NOOP")) none) #u)

    ;; any command: parts as for send-command (see imap/astring).
    (define (command what parts)
      (locked what parts (lambda (resp) #t)))

    ;; untagged responses that came unasked (EXISTS, EXPUNGE, FETCH
    ;;  flag changes...) since the last call.
    (define (take-events)
      (let ((l (reverse events)))
        (set! events (list:nil))
        l))

    (define (logout)
      (try
       (begin (locked "LOGOUT" (list (text "LOGOUT")) none) #u)
       except
       (:IMAP/Bye _) -> #u
       (:Reader/Closed _) -> #u)
      (conn.close))

    ;; --- fetching, as a stream ---

    ;; UID FETCH <items> for each of <uids>, <batch> at a time.  returns
    ;;  a closable generator (see make-closable-generator) of
    ;;  {uid items}, items (list (:tuple NAME value)), in the server's
    ;;  order.  e.g. items "(FLAGS RFC822.SIZE BODY.PEEK[])": the body
    ;;  comes back as "BODY[]".
    ;;
    ;; the connection is held until the stream ends or is closed: other
    ;;  threads wait, and this thread's other commands raise IMAP/Busy.
    ;;  closing early finishes only the batch in progress.
    (define (fetch-batch uids items batch)
      (define (take n l)
        (let loop ((n n) (l l) (acc (list:nil)))
          (if (or (= n 0) (null? l))
              (:tuple (reverse acc) l)
              (loop (- n 1) (cdr l) (list:cons (car l) acc)))))
      (check-busy)
      (let ((started? #f)
            (held? #f)
            (tag "")          ;; of the FETCH in progress, or ""
            (failure (maybe:no)))

        (define (acquire)
          (when (not held?)
            (mutex.acquire)
            (set! held? #t)
            (set! stream-thread *thread-id*)))

        (define (release)
          (when held?
            (set! held? #f)
            (set! stream-thread -1)
            (mutex.release)))

        ;; read up to the end of the FETCH in progress.  its untagged
        ;;  FETCHes are dropped.
        (define (finish-command)
          (when (> (string-length tag) 0)
            (let loop ()
              (match (read-response) with
                (imap-resp:status t cond _ text)
                -> (if (string=? t tag)
                       (begin
                         (set! tag "")
                         (match cond with
                           "OK" -> #u
                           "NO" -> (set! failure (maybe:yes (:tuple "NO" text)))
                           _    -> (set! failure (maybe:yes (:tuple "BAD" text)))))
                       (loop))
                _ -> (loop)))))

        (define (cleanup)
          (try
           (finish-command)
           finally
           (release))
          #u)

        (define gen
          (closable-gen emit
            (try
             (begin
               (acquire)
               (let batches ((left uids))
                 (when (not (null? left))
                   (match (take batch left) with
                     (:tuple these rest)
                     -> (begin
                          (set! tag (next-tag))
                          (send-command tag (list (text "UID FETCH") (text (imap/uid-set these)) (text items)))
                          (let loop ()
                            (match (read-response) with
                              (imap-resp:data ((imapv:num seq) (imapv:atom "FETCH") (imapv:list l)))
                              -> (let ((pairs (imap/pairs l)))
                                   (match (imap/item pairs "UID") with
                                     ;; (FETCH with no UID: a flag change from elsewhere)
                                     (maybe:yes (imapv:num uid)) -> (emit {uid=uid items=pairs})
                                     _ -> (push! events (imap-resp:data (list (imapv:num seq) (imapv:atom "FETCH") (imapv:list l)))))
                                   (loop))
                              (imap-resp:status t cond code text)
                              -> (if (string=? t tag)
                                     (begin
                                       (set! tag "")
                                       (match cond with
                                         "OK" -> #u
                                         "NO" -> (raise (:IMAP/No "FETCH" text))
                                         _    -> (raise (:IMAP/Bad "FETCH" text))))
                                     (begin (note-untagged (imap-resp:status t cond code text)) (loop)))
                              resp -> (begin (note-untagged resp) (loop))))
                          (batches rest))))))
             finally
             (cleanup))))

        {next = (lambda () (set! started? #t) (gen.next))
         ;; (an unstarted producer can't clean up after itself: but it
         ;;  hasn't taken anything yet, either.)
         close = (lambda () (gen.close) (when (not started?) (cleanup)))}))

    (define (fetch uids items)
      (fetch-batch uids items 100))

    ;; --- body of imap/client ---
    (match (read-response) with
      (imap-resp:status "*" cond code text)
      -> (begin
           (set! greeting text)
           (when (string=? cond "BYE") (raise (:IMAP/Bye text)))
           (check-code code))
      _ -> (raise (:IMAP/Protocol "no greeting")))
    (when (null? caps)
      (capability)
      #u)

    {capability = capability
     has-cap    = has-cap?
     starttls   = starttls
     login      = login
     list       = list-mailboxes
     select     = select
     examine    = examine
     status     = status
     search     = search
     fetch      = fetch
     fetch-batch = fetch-batch
     store      = store
     copy       = copy
     move       = move
     expunge    = expunge
     append     = append
     create     = create
     delete     = delete
     rename     = rename
     noop       = noop
     command    = command
     events     = take-events
     logout     = logout
     secure     = (lambda () tls?)
     greeting   = (lambda () greeting)
     }
    ))

;; connect to <host>:<port> (at <ip>, if not ""), with <security>.
;;  <verify> checks the server's certificates (see net/check-cert); it
;;  isn't used with (imap-security:none).  log in with the session's
;;  login.
(define (imap/connect host port ip security verify)
  (let ((ip (if (= 0 (string-length ip)) (net/resolve-host host) ip))
        (sock (doom/make (tcp4-sock))))
    (define (tls)
      (doom/stream (tls13/client sock host (list "imap") verify)))
    (sock.connect (address/make4 ip port))
    (let ((implicit? (eq? security (imap-security:implicit)))
          (s (imap/client (if implicit? (tls) (doom/stream sock)) implicit?)))
      (match security with
        (imap-security:starttls) -> (s.starttls tls)
        _ -> #u)
      s)))
