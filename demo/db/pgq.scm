;; -*- Mode: Irken -*-

;; run SQL on a PostgreSQL server (doom/db/postgres.scm), and print
;;  the results.
;;
;; $ PGPASSWORD=... demo/db/pgq -user sam -db test "select * from t"
;; $ demo/db/pgq -user sam -db test 'select * from t where n = $1' 42
;;
;; with no parameters the SQL may hold several statements (the simple
;;  query protocol); with parameters it must be one ($1, $2, ...).

(require "lib/basis.scm")
(require "lib/getopt.scm")
(require "lib/os.scm")
(require "doom/db/postgres.scm")

(define program-options
  (makeopt
   (flag 'k)
   (flag 'tls)
   (flag 'x)
   (arg 'host (string ""))
   (arg 'port (int 0))
   (arg 'ip (string ""))
   (arg 'root (string ""))
   (arg 'user (string ""))
   (arg 'db (string ""))
   (flag 'h)
   (pos 'sql (string ""))
   (pos* 'params (string ""))
   ))

(define (usage)
  (printf "run SQL on a PostgreSQL server.\n\n"
          "Usage: " sys.argv[0] " [options] -user <name> <sql> [<param> ...]\n"
          "  [-host <host>]  default localhost\n"
          "  [-port <n>]     default 5432\n"
          "  [-db <name>]    the database (default: the user name)\n"
          "  [-tls]          require TLS\n"
          "  [-root <pem>]   trust these certificates instead of the system's roots\n"
          "  [-k]            don't check the server's certificate (insecure!)\n"
          "  [-ip <addr>]    connect to this IPv4 address instead of looking up the host\n"
          "  [-x]            print each row as name: value lines\n"
          "  [-h]            print this usage.\n"
          "the password, if one is needed, is in $PGPASSWORD.\n")
  (%exit #f 2))

(define (die msg)
  (printf "pgq: " msg "\n")
  (%exit #f 1))

(define (show v)
  (match v with
    (maybe:yes s) -> s
    (maybe:no)    -> "NULL"))

;; a table, with each column as wide as its widest value.
(define (print-table res)
  (let ((names (map (lambda (c) c.name) res.columns))
        (rows (map (lambda (row) (map show (vector->list row))) res.rows))
        (widths (map string-length names)))
    (for-list row rows
      (set! widths (map2 max widths (map string-length row))))
    (define (line cells)
      (printf (string-join (map2 (lambda (w c) (rpad w c #\space)) widths cells) " | ") "\n"))
    (line names)
    (printf (string-join (map (lambda (w) (format (repeat w "-"))) widths) "-+-") "\n")
    (for-list row rows (line row))))

(define (print-expanded res)
  (let ((width (fold max 0 (map (lambda (c) (string-length c.name)) res.columns))))
    (for-list row res.rows
      (for-list pair (map2 (lambda (c v) (:tuple c.name v)) res.columns (vector->list row))
        (match pair with
          (:tuple name v) -> (printf (rpad width name) " : " (show v) "\n")))
      (printf "\n"))))

(define (print-result res expanded?)
  (when (not (null? res.columns))
    (if expanded? (print-expanded res) (print-table res)))
  (when (> (string-length res.tag) 0)
    (printf "(" res.tag ")\n")))

(define (run opts)
  (define (flag name) (match (get-bool-opt opts name) with (maybe:yes v) -> v _ -> #f))
  (define (str name) (match (get-string-opt opts name) with (maybe:yes v) -> v _ -> ""))
  (define (str-or name default) (let ((v (str name))) (if (= 0 (string-length v)) default v)))
  (let ((host (str-or 'host "localhost"))
        (port (match (get-int-opt opts 'port) with (maybe:yes n) -> (if (> n 0) n 5432) _ -> 5432))
        (user (str 'user))
        (sql (str 'sql))
        (params (match (getopt opts 'params) with
                  (maybe:yes (optval:list l))
                  -> (map (lambda (v) (match v with (optval:string s) -> (maybe:yes s) _ -> (maybe:no))) l)
                  _ -> (list:nil)))
        (tls (if (flag 'tls) (pg-tls:require) (pg-tls:disable)))
        (verify (if (flag 'k)
                    (lambda (ders) (parse-x509 (first ders)))
                    (net/check-cert (if (flag 'tls) (net/load-trust-anchors (str 'root)) (list:nil)) host))))
    (when (or (= 0 (string-length user)) (= 0 (string-length sql)))
      (usage))
    (let ((db (pg/connect host port (str 'ip) tls verify user (getenv "PGPASSWORD") (str-or 'db user))))
      (try
       (if (null? params)
           (for-list res (db.query sql)
             (print-result res (flag 'x)))
           (print-result (db.execute sql params) (flag 'x)))
       except
       (:PG/Error fields)
       -> (begin
            (for-list n (db.notices)
              (match n with (:tuple kind text) -> (printf kind ": " text "\n")))
            (raise (:PG/Error fields))))
      (for-list n (db.notices)
        (match n with (:tuple kind text) -> (printf kind ": " text "\n")))
      (db.close)
      (%exit #f 0))))

;; report failures in a word, not a backtrace.
(define (run* opts)
  (try
   (run opts)
   except
   (:PG/Error fields)      -> (die (pg/error-repr fields))
   (:PG/AuthFailed why)    -> (die (format "authentication: " why))
   (:PG/Unsupported why)   -> (die (format "not supported: " why))
   (:PG/Protocol why)      -> (die (format "protocol: " why))
   (:PG/BadArgument why)   -> (die why)
   (:PG/NoTLS)             -> (die "the server won't do TLS")
   (:Reader/Closed why)    -> (die why)
   (:TLS/ClientFailed why) -> (die (format "TLS: " why))
   (:Net/Failed why)       -> (die why)
   (:OSError n)            -> (die (format "error: " (strerror n)))
   (:X509/Invalid why)     -> (die why)
   (:SIG/NoPEMFound path)  -> (die (format "no certificates in " path))))

(define (main)
  (set-verbose-gc #f)
  (let ((opts (try
               (process-options program-options (rest (vector->list sys.argv)))
               except
               (:Getopt/MissingArg _ _) -> (usage)
               (:Getopt/UnknownArg _ _) -> (usage))))
    (when-maybe v (get-bool-opt opts 'h) (usage))
    (poller/fork (lambda () (run* opts)))
    (poller/wait-and-schedule)))

(main)
