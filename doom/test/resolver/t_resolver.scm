;; -*- Mode: Irken -*-

;; tests for doom/resolver, against fake_world.py (see run.sh).
;;
;;   t_resolver <base port>
;;
;; prints "ok" or "FAIL" for each check, and exits 1 if any failed.

(require "lib/basis.scm")
(require "doom/resolver/resolver.scm")

(define passed 0)
(define failed 0)

(define (check name ok?)
  (if ok?
      (begin (inc! passed) (printf "ok    " name "\n"))
      (begin (inc! failed) (printf "FAIL  " name "\n")))
  (flush)
  #u)

(define base 5300)

;; the fake world: 192.0.2.N is 127.0.0.1, port base+N.
(define (test-config)
  (let ((c (dns/config)))
    (set! c.hints (list (:tuple "a.root.test" "192.0.2.1")))
    (set! c.timeout 300)
    (set! c.addr-of (lambda (ip)
                      (match (string-split ip #\.) with
                        ("192" "0" "2" n) -> (:tuple "127.0.0.1" (+ base (string->int n)))
                        _ -> (:tuple "127.0.0.2" 9))))
    c))

(define (ips res)
  (sort string<?
        (filter (lambda (s) (> (string-length s) 0))
                (map (lambda (rr) (match rr.data with (dnsrr:a v) -> (dns/ip-of v) _ -> "")) res.answers))))

(define (ips=? res l)
  (and (eq? res.status (dns-status:answer))
       (string=? (string-join (ips res) ",") (string-join l ","))))

(define (traced? res text)
  (some? (lambda (line) (>= (string-find text line) 0)) res.trace))

(define (failed? res)
  (match res.status with (dns-status:failed _) -> #t _ -> #f))

(define (show res)
  (printf "      " (dns-status-repr res.status) "\n")
  (for-list line res.trace (printf "        " line "\n")))

;; resolve with fresh caches until the trace says <text> (the server
;;  order is random): did it ever, and was every answer right?
(define (sometimes r name qtype text ok?)
  (let loop ((n 0) (seen #f) (all-ok #t))
    (if (= n 12)
        (:tuple seen all-ok)
        (begin
          (r.flush)
          (let ((res (r.resolve name qtype)))
            (loop (+ n 1) (or seen (traced? res text)) (and all-ok (ok? res))))))))

(define (t-basics r)
  (let ((res (r.resolve "www.example.test" (dnstype:a))))
    (check "an answer, two addresses" (ips=? res '("192.0.2.180" "192.0.2.80")))
    (check "... via the referrals" (and (traced? res "referral to test") (traced? res "referral to example.test")))
    (when (not (ips=? res '("192.0.2.180" "192.0.2.80"))) (show res)))
  (let ((res (r.resolve "WWW.Example.TEST." (dnstype:a))))
    (check "cached, whatever the case" (and (ips=? res '("192.0.2.180" "192.0.2.80")) (not (traced? res "asking")))))
  (let ((res (r.resolve "alias.example.test" (dnstype:a))))
    (check "an alias" (ips=? res '("192.0.2.180" "192.0.2.80")))
    (check "... and its name" (match res.cnames with ("www.example.test") -> #t _ -> #f)))
  (let ((res (r.resolve "nosuch.example.test" (dnstype:a))))
    (check "NXDOMAIN" (eq? res.status (dns-status:nxdomain))))
  (let ((res (r.resolve "nosuch.example.test" (dnstype:a))))
    (check "NXDOMAIN, cached" (and (eq? res.status (dns-status:nxdomain)) (traced? res "cached"))))
  (let ((res (r.resolve "txtonly.example.test" (dnstype:a))))
    (check "NODATA" (eq? res.status (dns-status:nodata))))
  (let ((res (r.resolve "txtonly.example.test" (dnstype:a))))
    (check "NODATA, cached" (and (eq? res.status (dns-status:nodata)) (traced? res "cached"))))
  (let ((res (r.resolve "txtonly.example.test" (dnstype:txt))))
    (check "TXT" (and (eq? res.status (dns-status:answer)) (= 1 (length res.answers)))))
  (let ((res (r.resolve "mx.example.test" (dnstype:mx))))
    (check "MX" (match res.answers with
                  ({data=(dnsrr:mx 10 "mail.example.test") ...}) -> #t
                  _ -> #f)))
  (let ((res (r.resolve "unknown.example.test" (dnstype:other 65280))))
    (check "an unknown type, carried as it is"
           (match res.answers with
             ({data=(dnsrr:other 65280 "\x0a\x0b\x0c\x0d") ...}) -> #t
             _ -> #f))))

(define (t-aliases r)
  (let ((res (r.resolve "out.example.test" (dnstype:a))))
    (check "an alias into a glueless zone" (ips=? res '("192.0.2.81")))
    (check "... looking up its nameserver" (traced? res "looking up the nameserver ns.helper.example.test")))
  (let ((res (r.resolve "loop1.example.test" (dnstype:a))))
    (check "an alias loop fails" (and (failed? res) (traced? res "loop"))))
  (let ((res (r.resolve "c1.example.test" (dnstype:a))))
    (check "12 aliases is too many" (and (failed? res) (traced? res "too many aliases"))))
  (let ((res (r.resolve "alias.example.test" (dnstype:cname))))
    (check "asking for the CNAME itself" (match res.answers with
                                           ({data=(dnsrr:cname "www.example.test") ...}) -> #t
                                           _ -> #f))))

(define (t-servers r)
  (match (sometimes r "www.lame.test" (dnstype:a) "lame: ns-a.lame.test" (lambda (res) (ips=? res '("192.0.2.82")))) with
    (:tuple seen ok) -> (begin
                          (check "a lame server is skipped" seen)
                          (check "... and the other answers" ok)))
  (match (sometimes r "www.slow.test" (dnstype:a) "timeout from 192.0.2.8" (lambda (res) (ips=? res '("192.0.2.83")))) with
    (:tuple seen ok) -> (begin
                          (check "a silent server times out" seen)
                          (check "... and the other answers" ok)))
  (let ((res (r.resolve "www.dead.test" (dnstype:a))))
    (check "the only server is silent: failed" (and (failed? res) (traced? res "timeout"))))
  (let ((res (r.resolve "www.cycle1.test" (dnstype:a))))
    (check "nameservers in a cycle: failed" (and (failed? res) (traced? res "dependency cycle"))))
  (let ((res (r.resolve "www.refused.test" (dnstype:a))))
    (check "REFUSED: failed" (and (failed? res) (traced? res "REFUSED"))))
  (let ((res (r.resolve "www.big.test" (dnstype:txt))))
    (check "a truncated reply, then TCP" (and (eq? res.status (dns-status:answer)) (= 20 (length res.answers))
                                              (traced? res "trying TCP"))))
  (let ((res (r.resolve "www.noedns.test" (dnstype:a))))
    (check "no EDNS: asked again without" (and (ips=? res '("192.0.2.85")) (traced? res "refused EDNS")))))

(define (t-paranoia)
  ;; a fresh resolver: nothing cached that the poison could hide behind
  (let ((r (dns/resolver (test-config))))
    (let ((res (r.resolve "x.poison.test" (dnstype:a))))
      (check "poison: its own answer" (ips=? res '("192.0.2.87"))))
    (let ((res (r.resolve "www.sub.poison.test" (dnstype:a))))
      (check "poison: glue for a name outside its zone is ignored" (traced? res "ignored glue for ns1.example.test")))
    (let ((res (r.resolve "www.example.test" (dnstype:a))))
      (check "poison: its extra answer wasn't cached" (ips=? res '("192.0.2.180" "192.0.2.80"))))
    (let ((res (r.resolve "ns1.example.test" (dnstype:a))))
      (check "poison: its extra address wasn't cached" (ips=? res '("192.0.2.3")))))
  (let ((r (dns/resolver (test-config))))
    (let ((res (r.resolve "www.spoof.test" (dnstype:a))))
      (check "a reply with the wrong id is ignored" (and (ips=? res '("192.0.2.88")) (traced? res "wrong id")))))
  (let ((r (dns/resolver (test-config))))
    (match (sometimes r "www.wrongq.test" (dnstype:a) "another question" (lambda (res) (ips=? res '("192.0.2.86")))) with
      (:tuple seen ok) -> (begin
                            (check "a reply to another question is refused" seen)
                            (check "... and the other server answers" ok))))
  (let ((c (test-config)))
    (set! c.caps #t)
    (let ((r (dns/resolver c)))
      (let ((res (r.resolve "www.example.test" (dnstype:a))))
        (check "0x20: mixed case, echoed" (ips=? res '("192.0.2.180" "192.0.2.80"))))
      (let ((res (r.resolve "www.nocase.test" (dnstype:a))))
        (check "0x20: a server that doesn't echo the case is refused" (and (failed? res) (traced? res "another question"))))))
  (let ((r (dns/resolver (test-config))))
    (let ((res (r.resolve "www.nocase.test" (dnstype:a))))
      (check "without 0x20, the same server is fine" (ips=? res '("192.0.2.89"))))))

(define (t-limits)
  (let ((c (test-config)))
    (set! c.max-queries 3)
    (let ((r (dns/resolver c)))
      (let ((res (r.resolve "www.glueless.test" (dnstype:a))))
        (check "a budget of 3 queries: too much work" (and (failed? res) (traced? res "too much work"))))))
  (let ((c (test-config)))
    (set! c.max-entries 5)
    (let ((r (dns/resolver c)))
      (let ((a (r.resolve "www.example.test" (dnstype:a)))
            (b (r.resolve "www.glueless.test" (dnstype:a)))
            (d (r.resolve "www.example.test" (dnstype:a))))
        (check "a tiny cache still works" (and (ips=? a '("192.0.2.180" "192.0.2.80")) (ips=? b '("192.0.2.81"))
                                               (ips=? d '("192.0.2.180" "192.0.2.80"))))
        (check "... and stays small" (<= (r.cache-size) 6)))))
  (let ((r (dns/resolver (test-config))))
    (check "a bad name" (failed? (r.resolve "a..b.test" (dnstype:a))))
    (check "a label too long" (failed? (r.resolve (format (repeat 64 "x") ".test") (dnstype:a))))))

;; ten threads at once, one resolver
(define (t-threads k)
  (let ((r (dns/resolver (test-config)))
        (names '("www.example.test" "www.glueless.test" "www.lame.test" "www.big.test" "www.noedns.test"
                 "alias.example.test" "out.example.test" "www.spoof.test" "www.slow.test" "x.poison.test"))
        (good 0)
        (done 0))
    (for-list name names
      (let ((name name))
        (poller/fork
         (lambda ()
           (let ((res (r.resolve name (dnstype:a))))
             (when (eq? res.status (dns-status:answer)) (inc! good))
             (inc! done)
             (when (= done 10)
               (check "10 threads at once" (= good 10))
               (k)))))))))

(define (finish)
  (printf (int passed) " passed, " (int failed) " failed\n")
  (%exit #f (if (= failed 0) 0 1)))

(define (main)
  (set-verbose-gc #f)
  (when (> (vector-length sys.argv) 1) (set! base (string->int sys.argv[1])))
  (poller/fork
   (lambda ()
     (let ((r (dns/resolver (test-config))))
       (t-basics r)
       (t-aliases r)
       (t-servers r)
       (t-paranoia)
       (t-limits)
       (t-threads finish))))
  (poller/wait-and-schedule))

(main)
