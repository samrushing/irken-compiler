;; -*- Mode: Irken -*-

;; tests for doom/mail/imap.scm, against the dovecot that run.sh starts.
;;
;;   t_imap <port> <cert.pem>      dovecot: STARTTLS on <port>, TLS on <port>+1
;;   t_imap <port> fake            fake_server.py, misbehaving
;;
;; prints "ok" or "FAIL" for each check, and exits 1 if any failed.

(require "lib/basis.scm")
(require "doom/mail/imap.scm")

(define passed 0)
(define failed 0)

(define (check name ok?)
  (if ok?
      (begin (inc! passed) (printf "ok    " name "\n"))
      (begin (inc! failed) (printf "FAIL  " name "\n")))
  (flush)
  #u)

(define port 2143)
(define cert "")

(define (insecure ders) (parse-x509 (first ders)))

(define (connect security)
  (imap/connect "localhost" (if (eq? security (imap-security:implicit)) (+ port 1) port)
                "127.0.0.1" security insecure))

(define (login security)
  (let ((s (connect security)))
    (s.login "sam" "hunter2")
    s))

;; the response kind of the IMAP error <thunk> raises, or "".
(define (imap-error thunk)
  (try
   (begin (thunk) "")
   except
   (:IMAP/No _ _)  -> "NO"
   (:IMAP/Bad _ _) -> "BAD"
   (:IMAP/Busy _)  -> "BUSY"))

(define (number-of items name)
  (match (imap/item items name) with
    (maybe:yes (imapv:num n)) -> n
    _ -> -1))

(define (status-of s mailbox item)
  (let loop ((l (s.status mailbox (list item))))
    (match l with
      () -> -1
      ((:tuple k n) . tl) -> (if (string=? k item) n (loop tl)))))

(define (message n extra)
  (format "From: sam@example.com\r\nTo: fred@example.com\r\nSubject: message " (int n) "\r\n\r\n"
          "this is message " (int n) ".\r\n" extra))

(define (now-ns) (timespec->nanoseconds (gettime/monotonic)))

;; --- connecting and logging in ---

(define (t-login)
  (let ((s (connect (imap-security:starttls))))
    (check "STARTTLS" (s.secure))
    (check "capabilities before login" (s.has-cap "IMAP4REV1"))
    (s.login "sam" "hunter2")
    (check "login (AUTHENTICATE PLAIN)" (s.has-cap "MOVE"))
    (s.logout))
  (let ((s (login (imap-security:implicit))))
    (check "TLS from the start" (s.secure))
    (s.logout))
  (let ((s (login (imap-security:none))))
    (check "plain text" (not (s.secure)))
    (s.logout))
  (check "wrong password: NO"
         (string=? "NO" (imap-error (lambda () (let ((s (connect (imap-security:starttls)))) (s.login "sam" "nope"))))))
  (when (> (string-length cert) 0)
    (let ((anchors (net/load-trust-anchors cert)))
      (let ((s (imap/connect "localhost" port "127.0.0.1" (imap-security:starttls) (net/check-cert anchors "localhost"))))
        (s.login "sam" "hunter2")
        (check "certificate checked" (s.secure))
        (s.logout))
      (check "certificate for another name"
             (try (begin (imap/connect "127.0.0.1" (+ port 1) "127.0.0.1" (imap-security:implicit) (net/check-cert anchors "127.0.0.1")) #f)
                  except (:TLS/ClientFailed _) -> #t)))))

;; --- mailboxes ---

(define (t-mailboxes s)
  (let ((names (map (lambda (mb) mb.name) (s.list "" "*"))))
    (check "LIST" (and (member? "INBOX" names string=?) (member? "Trash" names string=?))))
  (s.create "Archive")
  (s.create "Test/Sub")
  (s.create "My \"Box\" \\ here")
  (let ((names (map (lambda (mb) mb.name) (s.list "" "*"))))
    (check "CREATE" (member? "Archive" names string=?))
    (check "CREATE in a hierarchy" (member? "Test/Sub" names string=?))
    (check "a name with quotes and a backslash" (member? "My \"Box\" \\ here" names string=?)))
  (s.rename "Test/Sub" "Test/Renamed")
  (check "RENAME" (member? "Test/Renamed" (map (lambda (mb) mb.name) (s.list "" "Test/*")) string=?))
  (s.delete "Test/Renamed")
  (s.delete "My \"Box\" \\ here")
  (check "DELETE" (null? (s.list "" "Test/Renamed")))
  (check "the delimiter" (match (s.list "" "INBOX") with (mb) -> (string=? mb.delimiter "/") _ -> #f))
  (check "SELECT a missing mailbox: NO" (string=? "NO" (imap-error (lambda () (s.select "Nowhere"))))))

;; --- APPEND: the messages for the rest ---

(define big-body
  (let ((line "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ{}()\"\\ []\r\n"))
    (string-concat (n-of 15000 line))))

(define (t-append s)
  (let ((uids (list:nil))
        (t0 (now-ns)))
    (for-range i 500
      (push! uids (s.append "INBOX" (list) (message (+ i 1) ""))))
    (check "500 APPENDs, with UIDs" (and (= 500 (length uids)) (every? (lambda (u) (> u 0)) uids)))
    (check "... in under 10 seconds" (< (- (now-ns) t0) 10000000000)))
  (let ((uid (s.append "INBOX" (list "\\Flagged" "\\Seen") (message 501 big-body))))
    (check "a 1MB APPEND" (> uid 0)))
  (s.append "INBOX" (list) "From: sam@example.com\r\nSubject: caf\xc3\xa9 \"quoted\" {5}\r\n\r\nna\xc3\xafve \xe2\x98\x95\r\n")
  (let ((sel (s.select "INBOX")))
    (check "SELECT: EXISTS" (= sel.exists 502))
    (check "SELECT: UIDVALIDITY" (> sel.uidvalidity 0))
    (check "SELECT: UIDNEXT" (= sel.uidnext 503))
    (check "SELECT: FLAGS" (member? "\\Seen" sel.flags string=?))
    (check "SELECT: read-write" (not sel.read-only)))
  (check "STATUS" (= 502 (status-of s "INBOX" "MESSAGES")))
  (check "STATUS UNSEEN" (= 501 (status-of s "INBOX" "UNSEEN"))))

;; --- searching and fetching ---

(define (t-fetch s)
  (let ((all (s.search "ALL")))
    (check "SEARCH ALL" (= 502 (length all)))
    (check "SEARCH FLAGGED" (= 1 (length (s.search "FLAGGED"))))
    (check "SEARCH SUBJECT" (= 1 (length (s.search "SUBJECT \"message 77\""))))

    ;; all of them, 100 at a time
    (let ((n 0) (in-order #t) (last 0) (sizes-ok #t))
      (for-closing m (s.fetch all "(UID FLAGS RFC822.SIZE)")
        (inc! n)
        (when (<= m.uid last) (set! in-order #f))
        (set! last m.uid)
        (when (<= (number-of m.items "RFC822.SIZE") 0) (set! sizes-ok #f)))
      (check "FETCH 502, in 6 batches" (= n 502))
      (check "... in UID order" in-order)
      (check "... with sizes" sizes-ok))

    ;; break early: only the batch in progress is finished.
    (let ((n 0) (t0 (now-ns)))
      (for-closing m (s.fetch-batch all "(UID BODY.PEEK[])" 20)
        (inc! n)
        (when (= n 3) (break)))
      (check "FETCH: break after 3" (= n 3))
      (check "... quickly" (< (- (now-ns) t0) 2000000000)))
    (check "usable after break" (= 1 (length (s.search "FLAGGED"))))

    ;; bodies, as literals: the 1MB one, intact
    (let ((big (first (s.search "FLAGGED")))
          (body ""))
      (for-closing m (s.fetch (list big) "(UID BODY.PEEK[TEXT])")
        (set! body (imap/item-string m.items "BODY[TEXT]")))
      (check "a 1MB body" (string=? body (format "this is message 501.\r\n" big-body))))

    ;; 8-bit, and an ENVELOPE with quotes and {5} in its subject
    (let ((uid (last all)) (body "") (subject ""))
      (for-closing m (s.fetch (list uid) "(UID BODY.PEEK[TEXT] ENVELOPE)")
        (set! body (imap/item-string m.items "BODY[TEXT]"))
        (match (imap/item m.items "ENVELOPE") with
          (maybe:yes (imapv:list (_ (imapv:str subj) . _))) -> (set! subject subj)
          _ -> #u))
      (check "8-bit body" (string=? body "na\xc3\xafve \xe2\x98\x95\r\n"))
      (check "ENVELOPE subject" (string=? subject "caf\xc3\xa9 \"quoted\" {5}")))

    ;; BODY[HEADER.FIELDS (SUBJECT)]: a section with spaces and parens
    (let ((h ""))
      (for-closing m (s.fetch (list (first all)) "(UID BODY.PEEK[HEADER.FIELDS (SUBJECT)])")
        (set! h (imap/item-string m.items "BODY[HEADER.FIELDS (SUBJECT)]")))
      (check "BODY[HEADER.FIELDS (SUBJECT)]" (string=? h "Subject: message 1\r\n\r\n")))

    (check "FETCH bad items: BAD" (string=? "BAD" (imap-error (lambda () (for-closing m (s.fetch all "(NOSUCHITEM)") #u)))))
    (check "usable after BAD" (= 502 (length (s.search "ALL"))))

    ;; a command inside the loop: IMAP/Busy
    (let ((busy ""))
      (for-closing m (s.fetch all "(UID)")
        (set! busy (imap-error (lambda () (s.noop))))
        (break))
      (check "a command inside the loop: IMAP/Busy" (string=? busy "BUSY")))
    (check "usable after IMAP/Busy" (= 502 (length (s.search "ALL"))))

    ;; never read, closed
    (let ((g (s.fetch all "(UID)")))
      (g.close))
    (check "closed unread, usable after" (= 502 (length (s.search "ALL"))))
    ))

;; --- changing things ---

(define (t-change s)
  (let ((all (s.search "ALL")))
    (s.store (list (nth all 0) (nth all 1) (nth all 2)) "+FLAGS" (list "\\Answered"))
    (check "STORE +FLAGS" (= 3 (length (s.search "ANSWERED"))))
    (s.store (list (nth all 1)) "-FLAGS" (list "\\Answered"))
    (check "STORE -FLAGS" (= 2 (length (s.search "ANSWERED"))))
    (s.copy (list (nth all 10) (nth all 11)) "Trash")
    (check "COPY" (= 2 (status-of s "Trash" "MESSAGES")))
    (s.move (list (nth all 20) (nth all 21) (nth all 22)) "Archive")
    (check "MOVE: there" (= 3 (status-of s "Archive" "MESSAGES")))
    (check "MOVE: gone from here" (= 499 (length (s.search "ALL"))))
    (s.store (list (nth all 30) (nth all 31)) "+FLAGS" (list "\\Deleted"))
    (s.store (list (nth all 32)) "+FLAGS" (list "\\Deleted"))
    (s.expunge (list (nth all 30) (nth all 31)))
    (check "UID EXPUNGE: just those" (= 497 (length (s.search "ALL"))))
    (check "... not the other \\Deleted" (= 1 (length (s.search "DELETED"))))
    (s.store (list (nth all 32)) "-FLAGS" (list "\\Deleted"))
    (let ((ex (s.examine "INBOX")))
      (check "EXAMINE: read-only" ex.read-only))
    (s.select "INBOX")
    #u))

;; --- events: another connection adds a message ---

(define (t-events s)
  (s.events)
  (let ((other (login (imap-security:starttls))))
    (other.append "INBOX" (list) (message 999 ""))
    (other.logout))
  (s.noop)
  (check "EXISTS from another connection"
         (some? (lambda (e) (match e with
                              (imap-resp:data (_ (imapv:atom "EXISTS"))) -> #t
                              _ -> #f))
                (s.events))))

;; --- threads: another thread's command waits for the stream ---

(define (t-threads s k)
  (let ((order (list:nil))
        (all (s.search "ALL")))
    (poller/fork
     (lambda ()
       (timeq/sleep 50)
       (s.noop)
       (push! order "noop")
       (check "another thread's command waits for the stream"
              (string=? (string-join (reverse order) ",") "fetch,noop"))
       (k)))
    ;; (slowly at first: the other thread tries while the stream is open.)
    (let ((n 0))
      (for-closing m (s.fetch-batch all "(UID)" 50)
        (inc! n)
        (when (< n 10) (timeq/sleep 20))))
    (push! order "fetch")))

;; --- fake_server.py ---

(define (t-fake)
  (define (attempt security)
    (try
     (let ((s (imap/connect "localhost" port "127.0.0.1" security insecure)))
       (s.login "sam" "pa\"ss w\xc3\xb6rd")
       (s.logout)
       "ok")
     except
     (:IMAP/Protocol why) -> why
     (:IMAP/Bye why) -> (format "BYE " why)
     (:IMAP/No _ why) -> (format "NO " why)
     (:IMAP/NoTLS) -> "NoTLS"))
  (check "LOGIN, with a literal and no LITERAL+" (string=? (attempt (imap-security:none)) "ok"))
  (check "plain text after the STARTTLS reply" (string=? (attempt (imap-security:starttls)) "data received after the STARTTLS reply"))
  (check "STARTTLS not offered" (string=? (attempt (imap-security:starttls)) "NoTLS"))
  (check "BYE greeting" (string=? (attempt (imap-security:none)) "BYE too busy"))
  (check "garbage" (starts-with (attempt (imap-security:none)) "bad response")))

(define (finish)
  (printf (int passed) " passed, " (int failed) " failed\n")
  (%exit #f (if (= failed 0) 0 1)))

(define (main)
  (set-verbose-gc #f)
  (when (> (vector-length sys.argv) 1) (set! port (string->int sys.argv[1])))
  (poller/fork
   (lambda ()
     (try
      (if (and (> (vector-length sys.argv) 2) (string=? sys.argv[2] "fake"))
          (begin (t-fake) (finish))
          (begin
            (when (> (vector-length sys.argv) 2) (set! cert sys.argv[2]))
            (t-login)
            (let ((s (login (imap-security:starttls))))
              (t-mailboxes s)
              (t-append s)
              (t-fetch s)
              (t-change s)
              (t-events s)
              (t-threads s (lambda () (s.logout) (finish))))))
      except
      (:IMAP/No what text)    -> (begin (printf "FAIL  unexpected NO to " what ": " text "\n") (inc! failed) (finish))
      (:IMAP/Bad what text)   -> (begin (printf "FAIL  unexpected BAD to " what ": " text "\n") (inc! failed) (finish))
      (:IMAP/Protocol why)    -> (begin (printf "FAIL  protocol: " why "\n") (inc! failed) (finish))
      (:IMAP/Bye why)         -> (begin (printf "FAIL  BYE: " why "\n") (inc! failed) (finish))
      (:TLS/ClientFailed why) -> (begin (printf "FAIL  TLS: " why "\n") (inc! failed) (finish))
      (:Reader/Closed why)    -> (begin (printf "FAIL  " why "\n") (inc! failed) (finish))
      )))
  (poller/wait-and-schedule))

(main)
