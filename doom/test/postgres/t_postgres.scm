;; -*- Mode: Irken -*-

;; tests for doom/db/postgres.scm, against the server that run.sh sets up.
;;
;;   t_postgres <port>
;;   t_postgres <port> tls <cert.pem>
;;   t_postgres <port> fake
;;
;; prints "ok" or "FAIL" for each check, and exits 1 if any failed.
;;  with "tls", everything is done over TLS, and the server's
;;  certificate (cert.pem) is checked too.  with "fake", <port> is
;;  fake_server.py instead, and only the SSLRequest checks are run.

(require "lib/basis.scm")
(require "doom/db/postgres.scm")

(define passed 0)
(define failed 0)

(define (check name ok?)
  (if ok?
      (begin (inc! passed) (printf "ok    " name "\n"))
      (begin (inc! failed) (printf "FAIL  " name "\n")))
  (flush)
  #u)

(define port 5433)
(define tls? #f)

;; lists of equal length with equal elements
(define (list=? a b same?)
  (and (= (length a) (length b))
       (every? (lambda (x) x) (map2 same? a b))))

(define (insecure ders) (parse-x509 (first ders)))

(define (connect user password)
  (pg/connect "localhost" port "127.0.0.1"
              (if tls? (pg-tls:require) (pg-tls:disable))
              insecure user password "test"))

(define (value res row col)
  (match (let ((r (nth res.rows row))) r[col]) with
    (maybe:yes s) -> s
    (maybe:no)    -> "NULL"))

;; the SQLSTATE of the PG/Error that <thunk> raises, or "" if none.
(define (sqlstate thunk)
  (try
   (begin (thunk) "")
   except
   (:PG/Error fields) -> (pg/error-field fields #\C)))

;; --- logging in ---

(define (t-auth)
  (let ((db (connect "trusty" "")))
    (check "trust" (string=? (value (first (db.query "select current_user")) 0 0) "trusty"))
    (db.close))
  (let ((db (connect "clear" "clearpw")))
    (check "cleartext password" (string=? (value (first (db.query "select current_user")) 0 0) "clear"))
    (db.close))
  (let ((db (connect "sam" "hunter2")))
    (check "scram-sha-256" (string=? (value (first (db.query "select current_user")) 0 0) "sam"))
    (check "server_version parameter"
           (match (db.parameter "server_version") with (maybe:yes _) -> #t _ -> #f))
    (check "backend pid" (= (db.pid) (string->int (value (first (db.query "select pg_backend_pid()")) 0 0))))
    (db.close))
  (check "wrong scram password: 28P01" (string=? (sqlstate (lambda () (connect "sam" "nope"))) "28P01"))
  (check "wrong cleartext password: 28P01" (string=? (sqlstate (lambda () (connect "clear" "nope"))) "28P01"))
  (check "no such database: 3D000"
         (string=? (sqlstate (lambda () (pg/connect "localhost" port "127.0.0.1" (pg-tls:disable) insecure "sam" "hunter2" "nosuch")))
                   "3D000"))
  (when (not tls?)
    (check "hostnossl reject: 28000" (string=? (sqlstate (lambda () (connect "tlsonly" "tlspw"))) "28000")))
  (check "md5 is refused"
         (try (begin (connect "old" "oldpw") #f)
              except (:PG/Unsupported why) -> (starts-with why "md5")))
  )

;; --- queries ---

(define (t-query db)
  (let ((rs (db.query "select 1 as a, 'x' as b, null as c; select 2 union select 3 order by 1; create temp table t (n int, s text); insert into t values (1, 'one'), (2, 'two')")))
    (check "several statements" (= 4 (length rs)))
    (let ((r0 (nth rs 0)))
      (check "column names" (list=? (map (lambda (c) c.name) r0.columns) '("a" "b" "c") string=?))
      (check "column types (int4 text text)" (list=? (map (lambda (c) c.type) r0.columns) '(23 25 25) =))
      (check "values" (and (string=? (value r0 0 0) "1") (string=? (value r0 0 1) "x")))
      (check "NULL" (string=? (value r0 0 2) "NULL")))
    (check "two rows" (= 2 (let ((r (nth rs 1))) (length r.rows))))
    (check "tags" (list=? (map (lambda (r) r.tag) rs) '("SELECT 1" "SELECT 2" "CREATE TABLE" "INSERT 0 2") string=?)))
  (check "empty query" (list=? (map (lambda (r) r.tag) (db.query "")) '("") string=?))
  (check "unicode" (string=? (value (first (db.query "select 'naïve ☕'")) 0 0) "naïve ☕"))
  (let ((r (first (db.query "select g, repeat('x', 100) from generate_series(1, 20000) g"))))
    (check "20000 rows" (and (= 20000 (length r.rows)) (string=? (value r 19999 0) "20000"))))
  (let ((big (first (db.query "select repeat('abc', 1000000)"))))
    (check "a 3MB value" (= 3000000 (string-length (value big 0 0)))))
  )

(define (t-errors db)
  (check "syntax error: 42601" (string=? (sqlstate (lambda () (db.query "selec 1"))) "42601"))
  (check "usable after an error" (string=? (value (first (db.query "select 'still here'")) 0 0) "still here"))
  (check "error repr"
         (try (begin (db.query "select * from nosuch") #f)
              except (:PG/Error f) -> (string=? (pg/error-repr f) "ERROR 42P01: relation \"nosuch\" does not exist")))
  (check "error mid-batch: 22012" (string=? (sqlstate (lambda () (db.query "select 1; select 1/0; select 3"))) "22012"))
  (check "status idle" (eq? (db.status) #\I))
  (db.query "begin")
  (check "status in transaction" (eq? (db.status) #\T))
  (sqlstate (lambda () (db.query "select 1/0")))
  (check "status failed transaction" (eq? (db.status) #\E))
  (check "in a failed transaction: 25P02" (string=? (sqlstate (lambda () (db.query "select 1"))) "25P02"))
  (db.query "rollback")
  (check "status idle after rollback" (eq? (db.status) #\I))
  (check "COPY is refused"
         (try (begin (db.query "copy (select 1) to stdout") #f)
              except (:PG/Unsupported _) -> #t))
  (check "usable after COPY" (string=? (value (first (db.query "select 'ok'")) 0 0) "ok"))
  (check "COPY FROM STDIN is refused"
         (try (begin (db.query "copy t from stdin") #f)
              except
              (:PG/Unsupported _) -> #t
              (:PG/Error _) -> #t))
  (check "usable after COPY FROM" (string=? (value (first (db.query "select 'ok'")) 0 0) "ok"))
  )

(define (t-extended db)
  (let ((r (db.execute "select $1::int + $2::int, $3::text is null, $4::text" (list (maybe:yes "40") (maybe:yes "2") (maybe:no) (maybe:yes "it's \"quoted\"; drop table t")))))
    (check "parameters" (string=? (value r 0 0) "42"))
    (check "NULL parameter" (string=? (value r 0 1) "t"))
    (check "no quoting needed" (string=? (value r 0 2) "it's \"quoted\"; drop table t")))
  (check "execute: insert tag" (string=? (let ((r (db.execute "insert into t values ($1, $2)" (list (maybe:yes "3") (maybe:yes "three"))))) r.tag) "INSERT 0 1"))
  (check "execute: error" (string=? (sqlstate (lambda () (db.execute "select $1::int" (list (maybe:yes "x"))))) "22P02"))
  (check "execute: too few parameters: 08P01" (string=? (sqlstate (lambda () (db.execute "select $1::int, $2::int" (list (maybe:yes "1"))))) "08P01"))
  (check "usable after extended errors" (string=? (value (db.execute "select 'ok'" (list)) 0 0) "ok"))
  (let ((stmt (db.prepare "select s from t where n = $1")))
    (check "prepared, run 1" (string=? (value (stmt.run (list (maybe:yes "1"))) 0 0) "one"))
    (check "prepared, run 2" (string=? (value (stmt.run (list (maybe:yes "3"))) 0 0) "three"))
    (check "prepared, no rows" (null? (let ((r (stmt.run (list (maybe:yes "99"))))) r.rows)))
    (stmt.close)
    (check "closed" (not (string=? "" (sqlstate (lambda () (stmt.run (list (maybe:yes "1")))))))))
  (check "prepare error" (string=? (sqlstate (lambda () (db.prepare "selec"))) "42601"))
  )

(define (t-notices db)
  (db.notices)
  (db.query "do $$ begin raise notice 'hello %', 42; end $$")
  (db.query "listen chan; notify chan, 'payload'")
  (let ((ns (db.notices)))
    (check "a notice" (member? (:tuple "notice" "NOTICE 00000: hello 42") ns
                               (lambda (a b) (match a b with (:tuple k0 v0) (:tuple k1 v1) -> (and (string=? k0 k1) (string=? v0 v1))))))
    (check "a notification" (member? (:tuple "chan" "payload") ns
                                     (lambda (a b) (match a b with (:tuple k0 v0) (:tuple k1 v1) -> (and (string=? k0 k1) (string=? v0 v1))))))
    (check "notices are taken" (null? (db.notices)))))

;; --- streaming: rows, for-closing ---

(define (count-rows stream)
  (let ((n 0))
    (for-closing row stream (inc! n))
    n))

(define (now-ns) (timespec->nanoseconds (gettime/monotonic)))

(define (t-rows db)
  ;; all of them, a batch of 100 at a time
  (let ((r (db.rows-batch "select g, 'row ' || g from generate_series(1, 2000) g" (list) 100))
        (n 0)
        (in-order #t))
    (check "rows: columns" (list=? (map (lambda (c) c.name) r.columns) '("g" "?column?") string=?))
    (for-closing row r.rows
      (inc! n)
      (when (not (string=? (match row[0] with (maybe:yes v) -> v _ -> "") (int->string n)))
        (set! in-order #f)))
    (check "rows: 2000 in 20 batches, in order" (and (= n 2000) in-order)))
  (check "rows: usable after" (string=? (value (first (db.query "select 'ok'")) 0 0) "ok"))

  ;; break early out of a million rows: the portal is closed, not drained.
  (let ((t0 (now-ns))
        (r (db.rows-batch "select g from generate_series(1, 1000000) g" (list) 100))
        (n 0))
    (for-closing row r.rows
      (inc! n)
      (when (= n 5) (break)))
    (check "rows: break after 5 of 1000000" (= n 5))
    (check "rows: ... quickly" (< (- (now-ns) t0) 2000000000)))
  (check "rows: usable after break" (string=? (value (first (db.query "select 'ok'")) 0 0) "ok"))

  ;; parameters
  (let ((r (db.rows "select g from generate_series(1, $1::int) g" (list (maybe:yes "7")))))
    (check "rows: parameters" (= 7 (count-rows r.rows))))

  ;; never read, closed by hand
  (let ((r (db.rows "select g from generate_series(1, 1000) g" (list))))
    (r.rows.close))
  (check "rows: closed unread, usable after" (string=? (value (first (db.query "select 'ok'")) 0 0) "ok"))

  ;; an error part way: the rows before it, then PG/Error
  (let ((r (db.rows-batch "select 100 / (50 - g) from generate_series(1, 100) g" (list) 10))
        (n 0))
    (check "rows: error part way: 22012"
           (string=? (sqlstate (lambda () (for-closing row r.rows (inc! n)))) "22012"))
    (check "rows: ... after the rows before it" (>= n 40)))
  (check "rows: usable after an error" (string=? (value (first (db.query "select 'ok'")) 0 0) "ok"))

  ;; an error at the start
  (check "rows: syntax error: 42601" (string=? (sqlstate (lambda () (db.rows "selec" (list)))) "42601"))
  (check "rows: usable after a syntax error" (string=? (value (first (db.query "select 'ok'")) 0 0) "ok"))

  ;; a statement with no rows
  (let ((r (db.rows "create temp table tr (n int)" (list))))
    (check "rows: no columns" (null? r.columns))
    (check "rows: no rows" (= 0 (count-rows r.rows))))

  ;; a query inside the loop, on the same connection: PG/Busy
  (let ((r (db.rows "select g from generate_series(1, 10) g" (list)))
        (busy #f))
    (for-closing row r.rows
      (try
       (begin (db.query "select 1") #u)
       except
       (:PG/Busy _) -> (set! busy #t))
      (break))
    (check "rows: query inside the loop is PG/Busy" busy))
  (check "rows: usable after PG/Busy" (string=? (value (first (db.query "select 'ok'")) 0 0) "ok"))

  ;; in a transaction
  (db.query "begin")
  (let ((r (db.rows "select g from generate_series(1, 3000) g" (list))))
    (check "rows: in a transaction" (= 3000 (count-rows r.rows))))
  (check "rows: still in the transaction" (eq? (db.status) #\T))
  (db.query "commit")
  (check "rows: idle after commit" (eq? (db.status) #\I))
  )

;; another thread's query waits for the stream to end.
(define (t-rows-threads db k)
  (let ((order (list:nil)))
    (poller/fork
     (lambda ()
       (timeq/sleep 50)
       (db.query "select 1")
       (push! order "query")
       (check "rows: another thread's query waits for the stream"
              (string=? (string-join (reverse order) ",") "stream,query"))
       (k)))
    (let ((r (db.rows-batch "select g from generate_series(1, 20) g" (list) 5)))
      (for-closing row r.rows
        (timeq/sleep 10)))
    (push! order "stream")))

;; two threads: one waits in pg_sleep, the other cancels it.
(define (t-cancel db k)
  (let ((t0 (timespec->nanoseconds (gettime/monotonic))))
    (poller/fork (lambda () (timeq/sleep 300) (db.cancel)))
    (check "cancelled: 57014" (string=? (sqlstate (lambda () (db.query "select pg_sleep(10)"))) "57014"))
    (check "cancelled quickly" (< (- (timespec->nanoseconds (gettime/monotonic)) t0) 3000000000))
    (check "usable after cancel" (string=? (value (first (db.query "select 'ok'")) 0 0) "ok"))
    (k)))

;; two threads on one connection: the mutex keeps their queries apart.
(define (t-threads db k)
  (let ((done 0)
        (good 0))
    (for-range i0 5
      (poller/fork
       (let ((i i0)) ;; (for-range reuses one variable)
        (lambda ()
         (for-range j 20
           (let ((r (db.execute "select $1::int * 1000 + $2::int" (list (maybe:yes (int->string i)) (maybe:yes (int->string j))))))
             (when (= (string->int (value r 0 0)) (+ (* i 1000) j))
               (inc! good))))
         (inc! done)
         (when (= done 5)
           (check "5 threads, 100 queries, one connection" (= good 100))
           (k))))))))

(define (t-tls cert)
  (let ((db (connect "tlsonly" "tlspw")))
    (check "TLS" (string=? (value (first (db.query "select ssl::text from pg_stat_ssl where pid = pg_backend_pid()")) 0 0) "true"))
    (db.close))
  (let ((anchors (net/load-trust-anchors cert)))
    (let ((db (pg/connect "localhost" port "127.0.0.1" (pg-tls:require) (net/check-cert anchors "localhost") "sam" "hunter2" "test")))
      (check "TLS, certificate checked" (string=? (value (first (db.query "select 'ok'")) 0 0) "ok"))
      (db.close))
    (check "TLS, certificate for another name"
           (try
            (begin (pg/connect "127.0.0.1" port "127.0.0.1" (pg-tls:require) (net/check-cert anchors "127.0.0.1") "sam" "hunter2" "test") #f)
            except (:TLS/ClientFailed why) -> #t))))

;; fake_server.py answers the first SSLRequest with 'N', the second
;;  with 'S' and then some plain text.
(define (t-fake)
  (define (try-tls)
    (try
     (begin (pg/connect "localhost" port "127.0.0.1" (pg-tls:require) insecure "sam" "x" "test") "connected")
     except
     (:PG/NoTLS) -> "NoTLS"
     (:PG/Protocol why) -> why))
  (check "server refuses TLS" (string=? (try-tls) "NoTLS"))
  (check "plain text after 'S'" (string=? (try-tls) "data received after the SSLRequest answer")))

(define (finish)
  (printf (int passed) " passed, " (int failed) " failed\n")
  (%exit #f (if (= failed 0) 0 1)))

(define (main)
  (set-verbose-gc #f)
  (when (> (vector-length sys.argv) 1) (set! port (string->int sys.argv[1])))
  (when (> (vector-length sys.argv) 2) (set! tls? (string=? sys.argv[2] "tls")))
  (poller/fork
   (lambda ()
     (try
      (if (and (> (vector-length sys.argv) 2) (string=? sys.argv[2] "fake"))
       (begin (t-fake) (finish))
       (begin
        (t-auth)
        (when tls? (t-tls sys.argv[3]))
        (let ((db (connect "sam" "hunter2")))
          (t-query db)
          (t-errors db)
          (t-extended db)
          (t-notices db)
          (t-rows db)
          (t-cancel db (lambda () (t-threads db (lambda () (t-rows-threads db (lambda () (db.close) (finish))))))))))
      except
      (:PG/Error f)        -> (begin (printf "FAIL  unexpected: " (pg/error-repr f) "\n") (inc! failed) (finish))
      (:PG/Protocol why)   -> (begin (printf "FAIL  protocol: " why "\n") (inc! failed) (finish))
      (:PG/AuthFailed why) -> (begin (printf "FAIL  auth: " why "\n") (inc! failed) (finish))
      (:TLS/ClientFailed why) -> (begin (printf "FAIL  TLS: " why "\n") (inc! failed) (finish))
      (:Reader/Closed why) -> (begin (printf "FAIL  " why "\n") (inc! failed) (finish))
      )))
  (poller/wait-and-schedule))

(main)
