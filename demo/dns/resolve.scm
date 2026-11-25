;; -*- Mode: Irken -*-

;; resolve names with doom/resolver, starting from the root servers.
;;
;; $ demo/dns/resolve www.example.com
;; $ demo/dns/resolve -t mx -v example.com
;;
;; -v shows the trace: every server asked, every referral, and anything
;;  odd along the way.  -caps turns on 0x20.

(require "lib/basis.scm")
(require "lib/getopt.scm")
(require "doom/resolver/resolver.scm")

(define program-options
  (makeopt
   (flag 'v)
   (flag 'caps)
   (flag 'h)
   (arg 't (string "a"))
   (arg 'timeout (int 0))
   (pos* 'names (string ""))
   ))

(define (usage)
  (printf "resolve names, starting from the root servers.\n\n"
          "Usage: " sys.argv[0] " [-t <type>] [-v] <name> ...\n"
          "  [-t <type>]       a, aaaa, mx, ns, txt, soa, cname, ptr, srv... (default a), or TYPEnnn\n"
          "  [-v]              show the trace\n"
          "  [-caps]           0x20: mix the case of names, and check the echo\n"
          "  [-timeout <ms>]   for each attempt at a server (default 2000)\n"
          "  [-h]              print this usage.\n")
  (%exit #f 2))

(define (parse-type s)
  (let ((s (downcase s)))
    (if (starts-with s "type")
        (dnstype:other (string->int (substring s 4 (string-length s))))
        (try
         (name->dnstype s)
         except
         (:DNSUnknownType _) -> (begin (printf "unknown type: " s "\n") (usage))))))

(define (run opts)
  (let ((verbose? (match (get-bool-opt opts 'v) with (maybe:yes v) -> v _ -> #f))
        (qtype (parse-type (match (get-string-opt opts 't) with (maybe:yes v) -> v _ -> "a")))
        (names (match (getopt opts 'names) with
                 (maybe:yes (optval:list l)) -> (map (lambda (v) (match v with (optval:string s) -> s _ -> "")) l)
                 _ -> (list:nil)))
        (config (dns/config))
        (worst 0))
    (when (null? names) (usage))
    (set! config.caps (match (get-bool-opt opts 'caps) with (maybe:yes v) -> v _ -> #f))
    (match (get-int-opt opts 'timeout) with
      (maybe:yes n) -> (when (> n 0) (set! config.timeout n))
      _ -> #u)
    (let ((r (dns/resolver config)))
      (for-list name names
        (let ((res (r.resolve name qtype)))
          (when verbose?
            (for-list line res.trace (printf "  " line "\n")))
          (for-list alias res.cnames
            (printf name ": alias for " alias "\n"))
          (match res.status with
            (dns-status:answer)
            -> (for-list rr res.answers
                 (printf rr.name " " (int rr.ttl) " " (dnsrr-repr rr.data) "\n"))
            st -> (begin
                    (printf name " " (dnstype-name qtype) ": " (dns-status-repr st) "\n")
                    (set! worst (max worst (match st with (dns-status:failed _) -> 2 _ -> 1))))))))
    (%exit #f worst)))

(define (main)
  (set-verbose-gc #f)
  (let ((opts (try
               (process-options program-options (rest (vector->list sys.argv)))
               except
               (:Getopt/MissingArg _ _) -> (usage)
               (:Getopt/UnknownArg _ _) -> (usage))))
    (when-maybe v (get-bool-opt opts 'h) (usage))
    (poller/fork (lambda () (run opts)))
    (poller/wait-and-schedule)))

(main)
